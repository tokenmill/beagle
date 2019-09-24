(ns beagle.phrases
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [jsonista.core :as json]
            [beagle.validator :as validator]
            [beagle.annotation-merger :as merger]
            [beagle.dictionary-optimizer :as optimizer]
            [beagle.text-analysis :as text-analysis])
  (:import (java.util UUID ArrayList)
           (org.apache.lucene.document Document FieldType Field)
           (org.apache.lucene.index IndexOptions)
           (org.apache.lucene.monitor Monitor MonitorQuery HighlightsMatch MonitorConfiguration
                                      MonitorQuerySerializer HighlightsMatch$Hit)
           (org.apache.lucene.search PhraseQuery MatchAllDocsQuery)
           (org.apache.lucene.util BytesRef)
           (org.apache.lucene.analysis.miscellaneous PerFieldAnalyzerWrapper)))

(defn match->annotation [text ^Monitor monitor type-name ^HighlightsMatch match]
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

(defn annotate-text [^String text ^Monitor monitor field-names ^String type-name]
  (try
    (let [doc (Document.)]
      (doseq [field-name field-names]
        (.add doc (Field. ^String field-name text field-type)))
      (mapcat #(match->annotation text monitor type-name %)
              (.getMatches (.match monitor doc (HighlightsMatch/MATCHER)))))
    (catch Exception e
      (log/errorf "Failed to match text: '%s'" text)
      (.printStackTrace e))))

(defn prepare-synonyms [query-id {:keys [synonyms] :as dict-entry}]
  (map (fn [synonym]
         (-> dict-entry
             (assoc :text synonym)
             (dissoc :synonyms)
             (assoc :id (str (UUID/randomUUID)))
             (update-in [:meta] assoc :synonym? "true" :query-id query-id)))
       synonyms))

(defn defer-to-one-by-one-registration [^Monitor monitor monitor-queries]
  (doseq [mq monitor-queries]
    (try
      (.register monitor (doto (ArrayList.) (.add mq)))
      (catch Exception e
        (log/errorf "Failed to register query: '%s'" mq)
        (.printStackTrace e)))))

(defn save-queries-in-monitor [^Monitor monitor monitor-queries]
  (try
    (.register monitor ^Iterable monitor-queries)
    (catch Exception _
      (defer-to-one-by-one-registration monitor monitor-queries))))

(defn phrase->strings [dict-entry default-analysis-conf]
  (let [analyzer (text-analysis/get-string-analyzer dict-entry default-analysis-conf)]
    (into-array String (text-analysis/text->token-strings (:text dict-entry) analyzer))))

(defn dict-entry->monitor-query [{:keys [id text meta type slop] :as dict-entry} default-analysis-conf idx]
  (let [query-id (or id (str idx))
        metadata (reduce-kv (fn [m k v] (assoc m (name k) v)) {} (if type (assoc meta :_type type) meta))
        field-name (text-analysis/get-field-name dict-entry default-analysis-conf)
        strings (phrase->strings dict-entry default-analysis-conf)]
    (if (seq strings)
      (MonitorQuery. query-id
                     (if slop
                       (PhraseQuery. ^Integer slop ^String field-name #^"[Ljava.lang.String;" strings)
                       (PhraseQuery. ^String field-name #^"[Ljava.lang.String;" strings))
                     text
                     metadata)
      (log/warnf "Discarding the dictionary entry because no tokens: '%s'" dict-entry))))

(defn dict-entries->monitor-queries [dict-entries default-analysis-conf]
  (->> (map (fn [{id :id :as dict-entry} idx]
              (let [query-id (or id (str idx))]
                (cons
                  (dict-entry->monitor-query dict-entry default-analysis-conf idx)
                  (map #(dict-entry->monitor-query % default-analysis-conf nil)
                       (prepare-synonyms query-id dict-entry)))))
            dict-entries (range))
       (flatten)
       (remove nil?)))

(defn prepare-monitor [monitor dict-entries default-analysis-conf]
  (save-queries-in-monitor monitor (dict-entries->monitor-queries dict-entries default-analysis-conf)))

(def monitor-query-serializer
  (reify MonitorQuerySerializer
    (serialize [_ query]
      (BytesRef.
        (json/write-value-as-string
          {"query-id" (.getId query)
           "query"    (.getQueryString query)
           "metadata" (.getMetadata query)})))
    (deserialize [_ binary-value]
      (let [dq (json/read-value (io/reader (.bytes ^BytesRef binary-value)))]
        (MonitorQuery. (get dq "query-id")
                       (MatchAllDocsQuery.)
                       (get dq "query")
                       (get dq "metadata"))))))

(defn create-monitor [field-names-w-analyzers]
  (let [^MonitorConfiguration config (MonitorConfiguration.)
        per-field-analyzers (PerFieldAnalyzerWrapper.
                              (text-analysis/get-string-analyzer {} {}) field-names-w-analyzers)]
    (.setIndexPath config nil monitor-query-serializer)
    (Monitor. per-field-analyzers config)))

(defn field-name-analyzer-mappings
  "Creates a map with field names as keys and Lucene analyzers as values.
  Both field name and analyzer are decided based on the dictionary entry configuration.
  First group dictionary entries by field name. Then from every group of dictionary entries
  take the first entry and create an analyzer based on analysis configuration."
  [dictionary default-analysis-conf]
  (reduce (fn [acc [k v]]
            (assoc acc k (text-analysis/get-string-analyzer (first v) default-analysis-conf)))
          {}
          (group-by #(text-analysis/get-field-name % default-analysis-conf) dictionary)))

(defn setup-monitor
  "Setups the monitor with all the dictionary entries."
  [dictionary default-analysis-conf]
  (let [mappings-from-field-names-to-analyzers (field-name-analyzer-mappings dictionary default-analysis-conf)
        monitor (create-monitor mappings-from-field-names-to-analyzers)]
    (prepare-monitor monitor dictionary default-analysis-conf)
    {:monitor     monitor
     :field-names (keys mappings-from-field-names-to-analyzers)}))

(defn synonym-annotation? [annotation]
  (= "true" (get-in annotation [:meta "synonym?"])))

(defn meta-type? [annotation]
  (string? (get-in annotation [:meta "_type"])))

(defn post-process [annotation]
  (cond-> annotation
          (synonym-annotation? annotation) (assoc :dict-entry-id (get-in annotation [:meta "query-id"]))
          (meta-type? annotation) (update-in [:meta] dissoc "_type")))

(defn match [text monitor field-names type-name opts]
  (if (s/blank? text)
    []
    (let [annotations (map post-process (annotate-text text monitor field-names type-name))]
      (if (:merge-annotations? opts)
        (merger/merge-same-type-annotations annotations)
        annotations))))

(defn annotator
  "Creates an annotator function with for a given dictionary.
  Params:
  - dictionary: a list of dictionary entries as described in `beagle.schema`
  Options:
  - type-name: a string, defaults to \"PHRASE\"
  - validate-dictionary?: if set to true then validates the dictionary, default false
  - optimize-dictionary?: if set to true then optimizes dictionary before creating the monitor, default false
  - tokenizer: a keyword one of #{:standard :whitespace}, default :standard"
  [dictionary & {:keys [type-name validate-dictionary? optimize-dictionary? tokenizer]}]
  (when validate-dictionary? (validator/validate-dictionary dictionary))
  (let [dictionary (if optimize-dictionary? (optimizer/optimize dictionary) dictionary)
        type-name (if (s/blank? type-name) "PHRASE" type-name)
        {:keys [monitor field-names]} (setup-monitor dictionary {:tokenizer tokenizer})]
    (fn
      ([text] (match text monitor field-names type-name {}))
      ([text & {:as opts}] (match text monitor field-names type-name opts)))))

(defn highlighter
  ([dictionary] (highlighter dictionary {}))
  ([dictionary opts]
   (when (:validate-dictionary? opts) (validator/validate-dictionary dictionary))
   (let [dictionary (if (:optimize-dictionary? opts) (optimizer/optimize dictionary) dictionary)
         type-name (if (s/blank? (:type-name opts)) "PHRASE" (:type-name opts))
         {:keys [monitor field-names]} (setup-monitor dictionary {:tokenizer (:tokenizer opts)})]
     (fn
       ([text] (match text monitor field-names type-name {}))
       ([text opts] (match text monitor field-names type-name opts))))))
