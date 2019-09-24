(ns beagle.lucene-alpha
  (:require [clojure.string :as s]
            [clojure.tools.logging :as log]
            [beagle.monitor :as monitor]
            [beagle.text-analysis :as text-analysis])
  (:import (org.apache.lucene.monitor MonitorQuery QueryMatch Monitor)
           (org.apache.lucene.queryparser.classic QueryParser ParseException)
           (org.apache.lucene.document Document Field FieldType)
           (org.apache.lucene.index IndexOptions)))

(def ^FieldType field-type
  (doto (FieldType.)
    (.setTokenized true)
    (.setIndexOptions IndexOptions/DOCS_AND_FREQS)
    (.setStoreTermVectors true)
    (.setStoreTermVectorOffsets true)))

(defn match-text [^String text ^Monitor monitor field-names type-name]
  (let [doc (Document.)]
    (doseq [field-name field-names]
      (.add doc (Field. ^String field-name text field-type)))
    (map (fn [^QueryMatch query-match]
           (let [^MonitorQuery query (.getQuery monitor (.getQueryId query-match))
                 meta (.getMetadata query)]
             {:text          (.getQueryString query)
              :type          (or (get meta "_type") type-name)
              :dict-entry-id (.getQueryId query-match)
              :meta          (into {} meta)})) (.getMatches (.match monitor doc (QueryMatch/SIMPLE_MATCHER))))))

(defn dict-entry->monitor-queries [{:keys [id text meta type] :as dict-entry} default-analysis-conf idx]
  (try
    (let [query-id (or id (str idx))
          metadata (reduce-kv (fn [m k v] (assoc m (name k) v)) {} (if type (assoc meta :_type type) meta))]
      (MonitorQuery. query-id
                     (.parse (QueryParser.
                               (text-analysis/get-field-name dict-entry default-analysis-conf)
                               (text-analysis/get-string-analyzer dict-entry default-analysis-conf))
                             text)
                     text
                     metadata))
    (catch ParseException e
      (log/errorf "Failed to parse query: '%s' with exception '%s'" dict-entry e))
    (catch Exception e (log/errorf "Failed create query: '%s' with '%s'" dict-entry e))))

(defn dictionary->monitor-queries [dictionary default-analysis-conf]
  (remove nil?
          (map (fn [dict-entry idx]
                 (dict-entry->monitor-queries dict-entry default-analysis-conf idx))
               dictionary (range))))

(defn match-monitor [text monitor field-names type-name opts]
  (log/debugf "Match monitor with opts='%s'" opts)
  (if (s/blank? text)
    []
    (match-text text monitor field-names type-name)))

(defn annotator
  ([dictionary] (annotator dictionary {}))
  ([dictionary {:keys [type-name tokenizer]}]
   (let [type-name (if (s/blank? type-name) "QUERY" type-name)
         {:keys [monitor field-names]} (monitor/setup dictionary
                                                      {:tokenizer tokenizer}
                                                      dictionary->monitor-queries)]
     (fn
       ([text] (match-monitor text monitor field-names type-name {}))
       ([text opts] (match-monitor text monitor field-names type-name opts))))))
