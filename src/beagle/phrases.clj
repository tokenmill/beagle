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
           (org.apache.lucene.search MultiPhraseQuery$Builder FuzzyQuery)
           (org.apache.lucene.search.spans SpanNearQuery$Builder SpanTermQuery SpanMultiTermQueryWrapper)))

(defn filter-and-sort-ordered-hits [^String text ^String highlight-text ordered-hits]
  (->> ordered-hits
       (filter (fn [^HighlightsMatch$Hit hit]
                 (= highlight-text (let [s (.-startOffset hit)
                                         e (.-endOffset hit)]
                                     (subs text s e)))))
       (sort-by (fn [^HighlightsMatch$Hit hit] (.-startOffset hit)))))

(defn group-sequencial-ending
  "Groups a sequence taking only the last hit from a consecutive sub-sequence
   of terms, e.g. [1 2 3 6 7] => [3 7]"
  [spans-end-hits]
  (loop [[current-term & terms] spans-end-hits
         last-item nil
         current-seq []
         filtered-ends []]
    (if (nil? current-term)
      (conj filtered-ends (last current-seq))
      (if (nil? last-item)
        (recur terms current-term [current-term] (if (seq current-seq)
                                                   (conj filtered-ends (last current-seq))
                                                   filtered-ends))
        (if (= (inc (.-startPosition last-item)) (.-startPosition current-term))
          (recur terms current-term (conj current-seq current-term) filtered-ends)
          (recur terms current-term [current-term] (conj filtered-ends (last current-seq))))))))

(defn pair-begins-with-ends [spans-start-hits spans-end-hits]
  (let [grouped-ends (group-sequencial-ending spans-end-hits)]
    (loop [[start & starts-tail :as starts] spans-start-hits
           [end & ends-tail] grouped-ends
           pairs []]
      (if (or (nil? start) (nil? end))
        pairs
        (if (= start end)
          (recur starts ends-tail pairs)
          (recur (remove #(< (.-startPosition %) (.-startPosition end)) starts-tail)
                 ends-tail (conj pairs [start end])))))))

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

(defn merge-dict-entry-with-highlighter-opts
  "There are dictionary opts that do not contribute to text analysis, but contributes
  to querying. This function acts a single point in merging default highlighter opts
  to the dictionary entry."
  [dict-entry default-analysis-conf]
  (cond-> dict-entry
          (and (not (contains? dict-entry :slop))
               (contains? default-analysis-conf :slop))
          (assoc :slop (:slop default-analysis-conf))

          (and (not (contains? dict-entry :in-order?))
               (contains? default-analysis-conf :in-order?))
          (assoc :in-order? (:in-order? default-analysis-conf))))

(defn dict-entry->monitor-query [dict-entry default-analysis-conf idx]
  (let [field-name (text-analysis/get-field-name dict-entry default-analysis-conf)
        terms (dict-entry->terms dict-entry default-analysis-conf)
        {:keys [id text meta type slop in-order?]
         :as dict-entry} (merge-dict-entry-with-highlighter-opts dict-entry default-analysis-conf)
        query-id (or id (str idx))
        metadata (reduce (fn [m [k v]] (assoc m (name k) v)) {} (if type (assoc meta :_type type) meta))
        normalized-slop (when slop (max 0 (min slop Integer/MAX_VALUE)))]
    (if (seq terms)
      (if (or (and (and (number? slop) (< 0 slop)) in-order? (< 1 (count terms)))
              (:fuzzy? dict-entry))
        (MonitorQuery. query-id
                       (try
                         (let [ordered? (cond
                                          in-order? true
                                          (and (nil? in-order?) (:fuzzy? dict-entry)) true
                                          :else false)
                               snqb (SpanNearQuery$Builder. ^String field-name ordered?)]
                           (doseq [term terms]
                             (if (true? (:fuzzy? dict-entry))
                               (.addClause snqb (SpanMultiTermQueryWrapper.
                                                  (FuzzyQuery.
                                                    (Term. ^String field-name ^String term)
                                                    (or (:fuzziness dict-entry) 1))))
                               (.addClause snqb (SpanTermQuery. (Term. ^String field-name ^String term)))))
                           (when-not (= slop normalized-slop)
                             (log/warnf "Phrase slop '%s' normalized to '%s'" slop normalized-slop))
                           (when normalized-slop
                             (.setSlop snqb normalized-slop))
                           (.build snqb))
                         (catch Exception e (.printStackTrace e)))
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
  (->> dict-entries
       (mapcat (fn [idx dict-entry]
                 (let [query-id (or (get dict-entry :id) (str idx))]
                   (cons
                     (dict-entry->monitor-query dict-entry default-analysis-conf idx)
                     (map #(dict-entry->monitor-query % default-analysis-conf nil)
                          (prepare-synonyms query-id dict-entry)))))
               (range))
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
      if set to true text matching is case sensitive, default true
  - ascii-fold?
      if set to true before matching text is ascii folded, default false
  - stem?
      if set to true before matching text is stemmed, default false
  - stemmer
      a keyword one of #{:arabic :armenian :basque :catalan :danish :dutch :english :estonian
      :finnish :french :german :german2 :hungarian :irish :italian :kp :lithuanian :lovins
      :norwegian :porter :portuguese :romanian :russian :spanish :swedish :turkish}
      that specifies the stemmer algorithm, default :english
  - slop
      the max edit-distance for phrase matching, default 0
  - in-order?
      if set to true enforces phrase terms ordering in matches, default false"
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
