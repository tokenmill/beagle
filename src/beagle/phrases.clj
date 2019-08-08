(ns beagle.phrases
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [schema.core :as sch]
            [beagle.annotation-merger :as merger]
            [beagle.dictionary-optimizer :as optimizer]
            [beagle.schema :as schema])
  (:import (java.util UUID)
           (java.io PushbackReader)
           (org.apache.lucene.analysis.core LowerCaseFilter WhitespaceTokenizer)
           (org.apache.lucene.analysis.standard ClassicFilter StandardTokenizer)
           (org.apache.lucene.analysis Analyzer Analyzer$TokenStreamComponents Tokenizer)
           (org.apache.lucene.analysis.miscellaneous ASCIIFoldingFilter)
           (org.apache.lucene.analysis.tokenattributes CharTermAttribute)
           (org.apache.lucene.document Document FieldType Field)
           (org.apache.lucene.index IndexOptions)
           (org.apache.lucene.monitor Monitor MonitorQuery HighlightsMatch MonitorConfiguration
                                      MonitorQuerySerializer HighlightsMatch$Hit)
           (org.apache.lucene.search PhraseQuery)
           (org.apache.lucene.util BytesRef)))

(def analysis-keys [:ascii-fold? :case-sensitive?])

(def default-conf {:case-sensitive? true :ascii-fold? false})

(defn kw->tokenizer [kw]
  (case kw
    :standard (StandardTokenizer.)
    :whitespace (WhitespaceTokenizer.)
    (StandardTokenizer.)))

(defn analyzers [conf tokenizer-kw]
  (let [^Tokenizer tokenizer (kw->tokenizer tokenizer-kw)]
    (case conf
      #{} {:analyzer   (proxy [Analyzer] []
                         (createComponents [^String field-name]
                           (Analyzer$TokenStreamComponents.
                             tokenizer (ClassicFilter. tokenizer))))
           :field-name "text"}
      #{:lowercase} {:analyzer   (proxy [Analyzer] []
                                   (createComponents [^String field-name]
                                     (Analyzer$TokenStreamComponents.
                                       tokenizer (LowerCaseFilter. tokenizer))))
                     :field-name "text.lowercased"}
      #{:ascii-fold} {:analyzer   (proxy [Analyzer] []
                                    (createComponents [^String field-name]
                                      (Analyzer$TokenStreamComponents.
                                        tokenizer (ASCIIFoldingFilter. tokenizer))))
                      :field-name "text.ascii-folded"}
      #{:lowercase :ascii-fold} {:analyzer   (proxy [Analyzer] []
                                               (createComponents [^String field-name]
                                                 (Analyzer$TokenStreamComponents.
                                                   tokenizer (ASCIIFoldingFilter. (LowerCaseFilter. tokenizer)))))
                                 :field-name "text.ascii-folded-lowercased"})))

(defn conf->analyzers [{:keys [ascii-fold? case-sensitive?]
                        :or   {ascii-fold?     (:ascii-fold? default-conf)
                               case-sensitive? (:case-sensitive? default-conf)}}]
  (cond-> #{}
          (false? case-sensitive?) (conj :lowercase)
          (true? ascii-fold?) (conj :ascii-fold)))

(defn ^Analyzer get-string-analyzer [analysis-conf tokenizer-kw]
  (get-in (analyzers (conf->analyzers analysis-conf) tokenizer-kw) [:analyzer]))

(defn ^String get-field-name [analysis-conf tokenizer-kw]
  (get-in (analyzers (conf->analyzers analysis-conf) tokenizer-kw) [:field-name]))

(defn match->annotation [text monitor type-name ^HighlightsMatch match]
  (mapcat
    (fn [[_ hits]]
      (let [meta (.getMetadata (.getQuery monitor (.getQueryId match)))]
        (map (fn [hit]
               (let [
                     start-offset (.-startOffset ^HighlightsMatch$Hit hit)
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

(defn annotate-text [^String text ^Monitor monitor analysis-conf ^String type-name tokenizer-kw]
  (let [field-name (get-field-name analysis-conf tokenizer-kw)
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
  (doseq [portion (partition-all 4000 monitor-queries)]
    (try
      (.register monitor ^Iterable portion)
      (catch Exception e
        (.printStackTrace e)))))

(defn phrase->strings [dict-entry tokenizer-kw]
  (let [analyzer (get-string-analyzer dict-entry tokenizer-kw)
        token-stream (.tokenStream analyzer "not-important" ^String (:text dict-entry))
        ^CharTermAttribute termAtt (.addAttribute token-stream CharTermAttribute)]
    (.reset token-stream)
    (into-array String (reduce (fn [acc _]
                                 (if (.incrementToken token-stream)
                                   (conj acc (.toString termAtt))
                                   (do
                                     (.close token-stream)
                                     (reduced acc)))) [] (range)))))

(defn dict-entry->monitor-query [{:keys [id text meta type] :as dict-entry} tokenizer idx]
  (let [query-id (or id (str idx))
        metadata (reduce-kv (fn [m k v] (assoc m (name k) v)) {} (if type (assoc meta :_type type) meta))]
    (MonitorQuery. query-id
                   (PhraseQuery. (get-field-name dict-entry tokenizer) (phrase->strings dict-entry tokenizer))
                   text
                   metadata)))

(defn dict-entries->monitor-queries [dict-entries tokenizer]
  (flatten
    (map (fn [{id :id :as dict-entry} idx]
           (let [query-id (or id (str idx))]
             (cons
               (dict-entry->monitor-query dict-entry tokenizer idx)
               (map #(dict-entry->monitor-query % tokenizer nil) (prepare-synonyms query-id dict-entry)))))
         dict-entries (range))))

(defn prepare-monitor [monitor dict-entries tokenizer]
  (save-queries-in-monitor monitor (dict-entries->monitor-queries dict-entries tokenizer)))

(defn create-monitor [analysis-conf tokenizer]
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
                                                                                 :text (:query dq)) tokenizer))
                                        (:query dq)
                                        (:metadata dq))))))
    (Monitor. (get-string-analyzer analysis-conf tokenizer) config)))

(defn setup-monitors [dictionary tokenizer-kw]
  (reduce-kv (fn [acc _ v]
               (let [analysis-conf (select-keys (first v) analysis-keys)
                     monitor (create-monitor analysis-conf tokenizer-kw)]
                 (prepare-monitor monitor v tokenizer-kw)
                 (conj acc {:analysis-conf analysis-conf :monitor monitor})))
             [] (group-by conf->analyzers dictionary)))

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
        monitors (setup-monitors dictionary tokenizer)]
    (fn [text & {:keys [merge-annotations?]}]
      (if (s/blank? text)
        []
        (let [annotations (map post-process
                               (mapcat (fn [{:keys [monitor analysis-conf]}]
                                         (annotate-text text monitor analysis-conf type-name tokenizer)) monitors))]
          (if merge-annotations?
            (merger/merge-same-type-annotations annotations)
            annotations))))))
