(ns beagle.phrases
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [schema.core :as sch]
            [beagle.annotation-merger :as merger]
            [beagle.dictionary-optimizer :as optimizer]
            [beagle.schema :as schema]
            [beagle.text-analysis :as text-analysis])
  (:import (java.util UUID)
           (java.io PushbackReader)
           (org.apache.lucene.analysis.tokenattributes CharTermAttribute)
           (org.apache.lucene.document Document FieldType Field)
           (org.apache.lucene.index IndexOptions)
           (org.apache.lucene.monitor Monitor MonitorQuery HighlightsMatch MonitorConfiguration
                                      MonitorQuerySerializer HighlightsMatch$Hit)
           (org.apache.lucene.search PhraseQuery)
           (org.apache.lucene.util BytesRef)))

(defn match->annotation [text monitor type-name ^HighlightsMatch match]
  (mapcat
    (fn [[_ hits]]
      (let [meta (.getMetadata (.getQuery monitor (.getQueryId match)))]
        (map (fn [hit]
               (let [start-offset (.-startOffset ^HighlightsMatch$Hit hit)
                     end-offset (.-endOffset ^HighlightsMatch$Hit hit)]
                 {:text          (subs text start-offset end-offset)
                  :type          (or (get meta "_type") type-name)
                  :dict-entry-id (.getQueryId match)
                  :meta          (into {} meta)
                  :begin-offset  start-offset
                  :end-offset    end-offset})) hits)))
    (.getHits match)))

(def ^FieldType field-type
  (doto (FieldType.)
    (.setTokenized true)
    (.setIndexOptions IndexOptions/DOCS_AND_FREQS_AND_POSITIONS_AND_OFFSETS)
    (.setStoreTermVectors true)
    (.setStoreTermVectorOffsets true)))

(defn annotate-text [^String text ^Monitor monitor analysis-conf ^String type-name text-analysis-resources]
  (let [field-name (text-analysis/get-field-name analysis-conf text-analysis-resources)
        doc (doto (Document.)
              (.add (Field. ^String field-name text field-type)))
        matches (.getMatches (.match monitor doc (HighlightsMatch/MATCHER)))]
    (mapcat #(match->annotation (.get doc field-name) monitor type-name %) matches)))

(defn prepare-synonyms [query-id {:keys [synonyms] :as dict-entry}]
  (map (fn [synonym]
         (-> dict-entry
             (assoc :text synonym)
             (dissoc :synonyms)
             (assoc :id (str (UUID/randomUUID)))
             (update-in [:meta] assoc :synonym? "true" :query-id query-id)))
       synonyms))

(defn save-queries-in-monitor [^Monitor monitor monitor-queries]
  (try
    (.register monitor ^Iterable monitor-queries)
    (catch Exception e
      (.printStackTrace e))))

(defn phrase->strings [dict-entry text-analysis-resources]
  (let [analyzer (text-analysis/get-string-analyzer dict-entry text-analysis-resources)
        token-stream (.tokenStream analyzer "not-important" ^String (:text dict-entry))
        ^CharTermAttribute termAtt (.addAttribute token-stream CharTermAttribute)]
    (.reset token-stream)
    (into-array String (reduce (fn [acc _]
                                 (if (.incrementToken token-stream)
                                   (conj acc (.toString termAtt))
                                   (do
                                     (.close token-stream)
                                     (reduced acc)))) [] (range)))))

(defn dict-entry->monitor-query [{:keys [id text meta type] :as dict-entry} text-analysis-resources idx]
  (let [query-id (or id (str idx))
        metadata (reduce-kv (fn [m k v] (assoc m (name k) v)) {} (if type (assoc meta :_type type) meta))]
    (MonitorQuery. query-id
                   (PhraseQuery. (text-analysis/get-field-name dict-entry text-analysis-resources)
                                 (phrase->strings dict-entry text-analysis-resources))
                   text
                   metadata)))

(defn dict-entries->monitor-queries [dict-entries text-analysis-resources]
  (flatten
    (map (fn [{id :id :as dict-entry} idx]
           (let [query-id (or id (str idx))]
             (cons
               (dict-entry->monitor-query dict-entry text-analysis-resources idx)
               (map #(dict-entry->monitor-query % text-analysis-resources nil) (prepare-synonyms query-id dict-entry)))))
         dict-entries (range))))

(defn prepare-monitor [monitor dict-entries text-analysis-resources]
  (save-queries-in-monitor monitor (dict-entries->monitor-queries dict-entries text-analysis-resources)))

(defn create-monitor [analysis-conf text-analysis-resources]
  (let [^MonitorConfiguration config (MonitorConfiguration.)]
    (.setIndexPath config nil
                   (reify MonitorQuerySerializer
                     (serialize [_ query]
                       (BytesRef.
                         (str {:query-id (.getId query)
                               :query    (.getQueryString query)
                               :metadata (.getMetadata query)})))
                     (deserialize [_ binary-value]
                       (let [dq (edn/read (PushbackReader. (io/reader (.bytes binary-value))))]
                         (MonitorQuery. (:query-id dq)
                                        (PhraseQuery. "field" (phrase->strings (assoc analysis-conf
                                                                                 :text (:query dq))
                                                                               text-analysis-resources))
                                        (:query dq)
                                        (:metadata dq))))))
    (Monitor. (text-analysis/get-string-analyzer analysis-conf text-analysis-resources) config)))

(defn setup-monitors [dictionary text-analysis-resources]
  (reduce-kv (fn [acc _ v]
               (let [analysis-conf (select-keys (first v) text-analysis/analysis-keys)
                     monitor (create-monitor analysis-conf text-analysis-resources)]
                 (prepare-monitor monitor v text-analysis-resources)
                 (conj acc {:analysis-conf analysis-conf :monitor monitor})))
             [] (group-by text-analysis/conf->analyzers dictionary)))

(defn synonym-annotation? [annotation]
  (= "true" (get-in annotation [:meta "synonym?"])))

(defn meta-type? [annotation]
  (string? (get-in annotation [:meta "_type"])))

(defn post-process [annotation]
  (cond-> annotation
          (synonym-annotation? annotation) (assoc :dict-entry-id (get-in annotation [:meta "query-id"]))
          (meta-type? annotation) (update-in [:meta] dissoc "_type")))

(defn annotator [dictionary & {:keys [type-name optimize-dictionary? validate-dictionary? tokenizer]}]
  (when validate-dictionary? (sch/validate schema/Dictionary dictionary))
  (let [dictionary (if optimize-dictionary? (optimizer/optimize dictionary) dictionary)
        type-name (if (s/blank? type-name) "PHRASE" type-name)
        text-analysis-resources (text-analysis/analyzers tokenizer)
        monitors (setup-monitors dictionary text-analysis-resources)]
    (fn [text & {:keys [merge-annotations?]}]
      (if (s/blank? text)
        []
        (let [annotations (map post-process
                               (mapcat (fn [{:keys [monitor analysis-conf]}]
                                         (annotate-text text monitor analysis-conf type-name text-analysis-resources)) monitors))]
          (if merge-annotations?
            (merger/merge-same-type-annotations annotations)
            annotations))))))
