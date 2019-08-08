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
           (org.apache.lucene.monitor Monitor MonitorQuery HighlightsMatch MonitorConfiguration MonitorQuerySerializer HighlightsMatch$Hit)
           (org.apache.lucene.search PhraseQuery)
           (org.apache.lucene.util BytesRef)))

(def keyword-analysis-conf {:ascii-fold? false :case-sensitive? true})
(def lowercase-analysis-conf {:ascii-fold? false :case-sensitive? false})
(def ascii-folding-analysis-conf {:ascii-fold? true :case-sensitive? true})
(def lowercase-ascii-fold-analysis-conf {:ascii-fold? true :case-sensitive? false})

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

(def ^FieldType field-type
  (doto (FieldType.)
    (.setTokenized true)
    (.setIndexOptions IndexOptions/DOCS_AND_FREQS_AND_POSITIONS_AND_OFFSETS)
    (.setStoreTermVectors true)
    (.setStoreTermVectorOffsets true)))

(defn ^Document input-document [^String text tokenizer-kw]
  (doto (Document.)
    (.add (Field. (get-field-name keyword-analysis-conf tokenizer-kw) text field-type))
    (.add (Field. (get-field-name lowercase-analysis-conf tokenizer-kw) text field-type))
    (.add (Field. (get-field-name ascii-folding-analysis-conf tokenizer-kw) text field-type))
    (.add (Field. (get-field-name lowercase-ascii-fold-analysis-conf tokenizer-kw) text field-type))))

(defn matches [text ^Monitor monitor tokenizer-kw]
  (-> (.match monitor (input-document text tokenizer-kw) (HighlightsMatch/MATCHER))
      (.getMatches)))

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

(defn mark-text [^String text ^Monitor monitor ^String type-name tokenizer-kw]
  (->> (matches text monitor tokenizer-kw)
       (mapcat #(match->annotation text monitor type-name %))))

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

(defn phrase->strings [dict-entry tokenizer]
  (let [analyzer (get-string-analyzer dict-entry tokenizer)]
    (let [a (.tokenStream analyzer "not-important" ^String (:text dict-entry))
          ^CharTermAttribute termAtt (.addAttribute a CharTermAttribute)]
      (.reset a)
      (into-array String (reduce (fn [acc _] (if (.incrementToken a)
                                               (conj acc (.toString termAtt))
                                               (do
                                                 (.close a)
                                                 (reduced acc)))) [] (range))))))

(defn dict-entry->monitor-query [{:keys [id text meta type] :as dict-entry} tokenizer nr]
  (let [query-id (or id (str nr))
        metadata (reduce-kv (fn [m k v] (assoc m (name k) v)) {} (if type (assoc meta :_type type) meta))]
    (MonitorQuery. query-id
                   (PhraseQuery. (get-field-name dict-entry tokenizer) (phrase->strings dict-entry tokenizer))
                   text
                   metadata)))

(defn dict-entries->monitor-queries [dict-entries tokenizer]
  (flatten
    (map (fn [{id :id :as dict-entry} nr]
           (let [query-id (or id (str nr))]
             (cons
               (dict-entry->monitor-query dict-entry tokenizer nr)
               (map #(dict-entry->monitor-query % tokenizer nil) (prepare-synonyms query-id dict-entry)))))
         dict-entries (range))))

(defn prepare-monitor [monitor dict-entries tokenizer]
  (save-queries-in-monitor monitor (dict-entries->monitor-queries dict-entries tokenizer)))

(defn create-monitor [analysis-conf tokenizer]
  (let [^MonitorConfiguration config (MonitorConfiguration.)]
    (.setIndexPath config nil
                   (reify MonitorQuerySerializer
                     (serialize [this query]
                       (BytesRef.
                         (str {:query-id (.getId query)
                               :query    (.getQueryString query)
                               :metadata (.getMetadata query)})))
                     (deserialize [this binary-value]
                       (let [dq (edn/read (PushbackReader. (io/reader (.bytes binary-value))))]
                         (MonitorQuery. (:query-id dq)
                                        (PhraseQuery. "text" (phrase->strings (assoc analysis-conf
                                                                                :text (:query dq)) tokenizer))
                                        (:query dq)
                                        (:metadata dq))))))
    (Monitor. (get-string-analyzer analysis-conf tokenizer) config)))

(defn get-dictionary-entries [groups analysis-conf]
  (get groups (conf->analyzers analysis-conf)))

(defn setup-monitors [dictionary tokenizer-kw]
  (let [^Monitor kw-monitor (create-monitor keyword-analysis-conf tokenizer-kw)
        ^Monitor lowercased-monitor (create-monitor lowercase-analysis-conf tokenizer-kw)
        ^Monitor ascii-folded-monitor (create-monitor ascii-folding-analysis-conf tokenizer-kw)
        ^Monitor lowercased-ascii-folded-monitor (create-monitor lowercase-ascii-fold-analysis-conf tokenizer-kw)

        groups (group-by conf->analyzers dictionary)

        _ (prepare-monitor kw-monitor (get-dictionary-entries groups keyword-analysis-conf) tokenizer-kw)
        _ (prepare-monitor lowercased-monitor (get-dictionary-entries groups lowercase-analysis-conf) tokenizer-kw)
        _ (prepare-monitor ascii-folded-monitor (get-dictionary-entries groups ascii-folding-analysis-conf) tokenizer-kw)
        _ (prepare-monitor lowercased-ascii-folded-monitor (get-dictionary-entries groups lowercase-ascii-fold-analysis-conf) tokenizer-kw)]
    [kw-monitor lowercased-monitor ascii-folded-monitor lowercased-ascii-folded-monitor]))

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
                               (mapcat (fn [monitor] (mark-text text monitor type-name tokenizer)) monitors))]
          (if merge-annotations?
            (merger/merge-same-type-annotations annotations)
            annotations))))))
