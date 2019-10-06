(ns beagle.phrases
  (:require [clojure.string :as s]
            [clojure.tools.logging :as log]
            [beagle.validator :as validator]
            [beagle.annotation-merger :as merger]
            [beagle.dictionary-optimizer :as optimizer]
            [beagle.text-analysis :as text-analysis]
            [beagle.monitor :as monitor]
            [beagle.schema :refer [->Highlight ->DictionaryEntry]])
  (:import (java.util UUID)
           (org.apache.lucene.document Document FieldType Field)
           (org.apache.lucene.index IndexOptions Term)
           (org.apache.lucene.monitor Monitor MonitorQuery HighlightsMatch HighlightsMatch$Hit)
           (org.apache.lucene.search MultiPhraseQuery$Builder)
           (org.apache.lucene.search.spans SpanNearQuery$Builder SpanTermQuery)))

(defn match->annotation [text ^Monitor monitor type-name ^HighlightsMatch match]
  (mapcat
    (fn [[_ hits]]
      (let [meta (.getMetadata (.getQuery monitor (.getQueryId match)))]
        (map (fn [hit]
               (let [start-offset (.-startOffset ^HighlightsMatch$Hit hit)
                     end-offset (.-endOffset ^HighlightsMatch$Hit hit)]
                 (->Highlight
                   (subs text start-offset end-offset)
                   (or (get meta "_type") type-name)
                   (.getQueryId match)
                   (into {} meta)
                   start-offset
                   end-offset))) hits)))
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
              (.getMatches
                (.match monitor
                        #^"[Lorg.apache.lucene.document.Document;" (into-array Document [doc])
                        (HighlightsMatch/MATCHER))
                0)))
    (catch Exception e
      (log/errorf "Failed to match text: '%s'" text)
      (.printStackTrace e))))

(defn prepare-synonyms [query-id {:keys [synonyms] :as dict-entry}]
  (map (fn [synonym]
         (->DictionaryEntry
           synonym
           (:type dict-entry)
           (str (UUID/randomUUID))
           nil
           (:case-sensitive? dict-entry)
           (:ascii-fold? dict-entry)
           (:stem? dict-entry)
           (:stemmer dict-entry)
           (:slop dict-entry)
           (:tokenizer dict-entry)
           (assoc (:meta dict-entry)
             :synonym? "true" :query-id query-id)))
       synonyms))

(defn phrase->strings [dict-entry default-analysis-conf]
  (let [analyzer (text-analysis/get-string-analyzer dict-entry default-analysis-conf)]
    (into-array String (text-analysis/text->token-strings (:text dict-entry) analyzer))))

(defn dict-entry->monitor-query [{:keys [id text meta type slop in-order?] :as dict-entry} default-analysis-conf idx]
  (let [query-id (or id (str idx))
        metadata (reduce (fn [m [k v]] (assoc m (name k) v)) {} (if type (assoc meta :_type type) meta))
        field-name (text-analysis/get-field-name dict-entry default-analysis-conf)
        strings (phrase->strings dict-entry default-analysis-conf)
        normalized-slop (when slop (max 0 (min slop Integer/MAX_VALUE)))]
    (if (seq strings)
      (MonitorQuery. query-id
                     (if (and slop in-order?)
                       (let [snqb (SpanNearQuery$Builder. ^String field-name in-order?)]
                         (doseq [s strings]
                           (.addClause snqb (SpanTermQuery. (Term. ^String field-name ^String s))))
                         (when-not (= slop normalized-slop)
                           (log/warnf "Phrase slop '%s' normalized to '%s'" slop normalized-slop))
                         (.setSlop snqb normalized-slop)
                         (.build snqb))
                       (let [mpqb (MultiPhraseQuery$Builder.)]
                         (doseq [s strings]
                           (.add mpqb (Term. ^String field-name ^String s)))
                         (when slop
                           (when-not (= slop normalized-slop)
                             (log/warnf "Phrase slop '%s' normalized to '%s'" slop normalized-slop))
                           (.setSlop mpqb normalized-slop))
                         (.build mpqb)))
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

(defn highlighter
  "Creates a highlighter function with for a given dictionary.
  Params:
  - dictionary
      a list of dictionary entries as described in `beagle.schema`
  Opts:
  - type-name
      a string, defaults to \"PHRASE\"
  - validate-dictionary?
      if set to true then validates the dictionary, default false
  - optimize-dictionary?
      if set to true then optimizes dictionary before creating the monitor, default false
  - tokenizer
      a keyword one of #{:keyword :letter :standard :unicode-whitespace :whitespace}, default :standard"
  ([dictionary] (highlighter dictionary {}))
  ([dictionary opts]
   (when (:validate-dictionary? opts) (validator/validate-dictionary dictionary))
   (let [dictionary (if (:optimize-dictionary? opts) (optimizer/optimize dictionary) dictionary)
         type-name (if (s/blank? (:type-name opts)) "PHRASE" (:type-name opts))
         {:keys [monitor field-names]} (monitor/setup dictionary {:tokenizer (:tokenizer opts)}
                                                      dict-entries->monitor-queries)]
     (fn
       ([text] (match text monitor field-names type-name {}))
       ([text opts] (match text monitor field-names type-name opts))))))

(defn ^:deprecated annotator
  [dictionary & {:keys [type-name validate-dictionary? optimize-dictionary? tokenizer]}]
  (when validate-dictionary? (validator/validate-dictionary dictionary))
  (let [dictionary (if optimize-dictionary? (optimizer/optimize dictionary) dictionary)
        type-name (if (s/blank? type-name) "PHRASE" type-name)
        {:keys [monitor field-names]} (monitor/setup dictionary {:tokenizer tokenizer}
                                                     dict-entries->monitor-queries)]
    (fn
      ([text] (match text monitor field-names type-name {}))
      ([text & {:as opts}] (match text monitor field-names type-name opts)))))
