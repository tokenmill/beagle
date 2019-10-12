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

(defn filter-and-sort-ordered-hits [^String text ^String highlight-text ordered-hits]
  (->> ordered-hits
       (filter (fn [^HighlightsMatch$Hit hit]
                 (= highlight-text (let [s (.-startOffset hit)
                                         e (.-endOffset hit)]
                                     (subs text s e)))))
       (sort-by (fn [^HighlightsMatch$Hit hit] (.-startOffset hit)))))

(defn pair-begins-with-ends
  "FIXME: overlapping spans e.g. \"A A\" or 'A A B'"
  [spans-start-hits spans-end-hits]
  (map (fn [start end] [start end]) spans-start-hits spans-end-hits))

(defn ordered-hits->highlights
  "The default highlighter fails to handle SpanNearQuery: highlights are term highlights not the whole
  span highlights.
  The temporary workaround works as follows:
  1) find the very first hit
  2) find the very last hit
  3) assume that all spans begins and ends with the same terms
  4) collect all hits like the beginning
  5) collect all hits like the ending
  6) pair beginnings with endings and make one highlight per pair"
  [text type-name query-id metadata ordered-hits]
  (let [^HighlightsMatch$Hit first-hit (apply min-key #(.-startOffset ^HighlightsMatch$Hit %) ordered-hits)
        first-text (subs text (.-startOffset first-hit) (.-endOffset first-hit))
        ^HighlightsMatch$Hit last-hit (apply max-key #(.-startOffset ^HighlightsMatch$Hit %) ordered-hits)
        last-text (subs text (.-startOffset last-hit) (.-endOffset last-hit))
        spans-start-hits (filter-and-sort-ordered-hits text first-text ordered-hits)
        spans-end-hits (filter-and-sort-ordered-hits text last-text ordered-hits)
        normalized-metadata (dissoc metadata "_in-order")]
    (map (fn [[^HighlightsMatch$Hit span-start-hit ^HighlightsMatch$Hit span-end-hit]]
           (let [start-offset (.-startOffset span-start-hit)
                 end-offset (.-endOffset span-end-hit)]
             (->Highlight
               (subs text start-offset end-offset)
               (or (get meta "_type") type-name)
               query-id
               normalized-metadata
               start-offset
               end-offset))) (pair-begins-with-ends spans-start-hits spans-end-hits))))

(defn match->annotation [text ^Monitor monitor type-name ^HighlightsMatch match]
  (mapcat
    (fn [[_ hits]]
      (let [query-id (.getQueryId match)
            metadata (into {} (.getMetadata (.getQuery monitor query-id)))]
        (if (get metadata "_in-order")
          (ordered-hits->highlights text type-name query-id metadata hits)
          (map (fn [^HighlightsMatch$Hit hit]
                 (let [start-offset (.-startOffset hit)
                       end-offset (.-endOffset hit)]
                   (->Highlight
                     (subs text start-offset end-offset)
                     (or (get metadata "_type") type-name)
                     query-id
                     metadata
                     start-offset
                     end-offset))) hits))))
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

(defn dict-entry->terms [dict-entry default-analysis-conf]
  (let [analyzer (text-analysis/get-string-analyzer dict-entry default-analysis-conf)]
    (into-array String (text-analysis/text->token-strings (:text dict-entry) analyzer))))

(defn dict-entry->monitor-query [{:keys [id text meta type slop in-order?] :as dict-entry} default-analysis-conf idx]
  (let [query-id (or id (str idx))
        metadata (reduce (fn [m [k v]] (assoc m (name k) v)) {} (if type (assoc meta :_type type) meta))
        field-name (text-analysis/get-field-name dict-entry default-analysis-conf)
        terms (dict-entry->terms dict-entry default-analysis-conf)
        normalized-slop (when slop (max 0 (min slop Integer/MAX_VALUE)))]
    (if (seq terms)
      (if (and (and (number? slop) (< 0 slop)) in-order? (< 1 (count terms)))
        (MonitorQuery. query-id
                       (let [snqb (SpanNearQuery$Builder. ^String field-name in-order?)]
                         (doseq [s terms]
                           (.addClause snqb (SpanTermQuery. (Term. ^String field-name ^String s))))
                         (when-not (= slop normalized-slop)
                           (log/warnf "Phrase slop '%s' normalized to '%s'" slop normalized-slop))
                         (.setSlop snqb normalized-slop)
                         (.build snqb))
                       text
                       (assoc metadata "_in-order" true))
        (MonitorQuery. query-id
                       (let [mpqb (MultiPhraseQuery$Builder.)]
                         (doseq [s terms]
                           (.add mpqb (Term. ^String field-name ^String s)))
                         (when slop
                           (when-not (= slop normalized-slop)
                             (log/warnf "Phrase slop '%s' normalized to '%s'" slop normalized-slop))
                           (.setSlop mpqb normalized-slop))
                         (.build mpqb))
                       text
                       metadata))
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
      a list of dictionary entries as described in `beagle.schema/dict-entry`
  Opts:
  - type-name
      a string, defaults to \"PHRASE\"
  - validate-dictionary?
      if set to true then validates the dictionary, default false
  - optimize-dictionary?
      if set to true then optimizes dictionary before creating the monitor, default false
  - tokenizer
      a keyword one of #{:keyword :letter :standard :unicode-whitespace :whitespace}, default :standard
  - case-sensitive?
      if set to true then text matching is case sensitive, default true
  - ascii-fold?
      if set to true then before matching text is ascii folded, default false
  - stem?
      if set to true then before matching text is stemmed, default false
  - stemmer
      a keyword one of #{:arabic :armenian :basque :catalan :danish :dutch :english :estonian
      :finnish :french :german :german2 :hungarian :irish :italian :kp :lithuanian :lovins
      :norwegian :porter :portuguese :romanian :russian :spanish :swedish :turkish}
      that specifies the stemmer algorithm, default :english"
  ([dictionary] (highlighter dictionary {}))
  ([dictionary opts]
   (when (:validate-dictionary? opts) (validator/validate-dictionary dictionary))
   (let [dictionary (if (:optimize-dictionary? opts) (optimizer/optimize dictionary) dictionary)
         type-name (if (s/blank? (:type-name opts)) "PHRASE" (:type-name opts))
         {:keys [monitor field-names]} (monitor/setup dictionary opts dict-entries->monitor-queries)]
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
