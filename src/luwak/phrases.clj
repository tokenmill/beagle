(ns luwak.phrases
  (:gen-class)
  (:require [clojure.string :as s]
            [schema.core :as sch]
            [luwak.annotation-merger :as merger]
            [luwak.dictionary-optimizer :as optimizer]
            [luwak.schema :as schema])
  (:import (uk.co.flax.luwak Monitor MonitorQuery InputDocument MonitorQueryParser)
           (uk.co.flax.luwak.presearcher MatchAllPresearcher)
           (uk.co.flax.luwak.matchers HighlightingMatcher HighlightsMatch HighlightsMatch$Hit)
           (org.apache.lucene.queryparser.complexPhrase ComplexPhraseQueryParser)
           (org.apache.lucene.analysis.core WhitespaceTokenizer LowerCaseFilter WhitespaceAnalyzer)
           (org.apache.lucene.analysis Analyzer Analyzer$TokenStreamComponents Tokenizer)
           (org.apache.lucene.analysis.miscellaneous ASCIIFoldingFilter)
           (java.util UUID)))

(def keyword-analysis-conf {:ascii-fold? false :case-sensitive? true})
(def lowercase-analysis-conf {:ascii-fold? false :case-sensitive? false})
(def ascii-folding-analysis-conf {:ascii-fold? true :case-sensitive? true})
(def lowercase-ascii-fold-analysis-conf {:ascii-fold? true :case-sensitive? false})

(def default-conf {:case-sensitive? true :ascii-fold? false})

(def analyzers
  {#{}                       {:analyzer   (WhitespaceAnalyzer.)
                              :field-name "text"}
   #{:lowercase}             {:analyzer   (proxy [Analyzer] []
                                            (createComponents [^String field-name]
                                              (let [^Tokenizer tokenizer (WhitespaceTokenizer.)]
                                                (Analyzer$TokenStreamComponents.
                                                  tokenizer (LowerCaseFilter. tokenizer)))))
                              :field-name "text.lowercased"}
   #{:ascii-fold}            {:analyzer   (proxy [Analyzer] []
                                            (createComponents [^String field-name]
                                              (let [^Tokenizer tokenizer (WhitespaceTokenizer.)]
                                                (Analyzer$TokenStreamComponents.
                                                  tokenizer (ASCIIFoldingFilter. tokenizer)))))
                              :field-name "text.ascii-folded"}
   #{:lowercase :ascii-fold} {:analyzer   (proxy [Analyzer] []
                                            (createComponents [^String field-name]
                                              (let [^Tokenizer tokenizer (WhitespaceTokenizer.)]
                                                (Analyzer$TokenStreamComponents.
                                                  tokenizer (ASCIIFoldingFilter. (LowerCaseFilter. tokenizer))))))
                              :field-name "text.ascii-folded-lowercased"}})

(defn conf->analyzers [{:keys [ascii-fold? case-sensitive?]
                        :or {ascii-fold? (:ascii-fold? default-conf)
                             case-sensitive? (:case-sensitive? default-conf)}}]
  (cond-> #{}
          (false? case-sensitive?) (conj :lowercase)
          (true? ascii-fold?) (conj :ascii-fold)))

(defn get-string-analyzer [analysis-conf]
  (get-in analyzers [(conf->analyzers analysis-conf) :analyzer]))

(defn get-field-name [analysis-conf]
  (get-in analyzers [(conf->analyzers analysis-conf) :field-name]))

(defn group-into-phrases [matches]
  (remove empty?
          (loop [current-phrase []
                 phrases []
                 last-token {:startPosition 0}
                 [token & tokens] (sort-by :startPosition matches)]
            (if-not token
              (conj phrases current-phrase)
              (if (= 1 (- (:startPosition token) (:startPosition last-token)))
                (recur (conj current-phrase token) phrases token tokens)
                (recur [token] (conj phrases current-phrase) token tokens))))))

(defn ^InputDocument input-document [^String text]
  (-> (InputDocument/builder "doc1")
      (.addField (get-field-name keyword-analysis-conf)
                 text
                 (get-string-analyzer keyword-analysis-conf))
      (.addField (get-field-name lowercase-analysis-conf)
                 text
                 (get-string-analyzer lowercase-analysis-conf))
      (.addField (get-field-name ascii-folding-analysis-conf)
                 text
                 (get-string-analyzer ascii-folding-analysis-conf))
      (.addField (get-field-name lowercase-ascii-fold-analysis-conf)
                 text
                 (get-string-analyzer lowercase-ascii-fold-analysis-conf))
      (.build)))

(defn matches [text ^Monitor monitor]
  (-> (.match monitor (input-document text) HighlightingMatcher/FACTORY)
      (.getMatches "doc1")
      (.getMatches)))

(defn match->annotation [text monitor type-name ^HighlightsMatch match]
  (->> (.getFields match)
       (first)
       (.getHits match)
       (map (fn [^HighlightsMatch$Hit hit]
              {:startPosition (.-startPosition hit)
               :startOffset   (.-startOffset hit)
               :endPosition   (.-endPosition hit)
               :endOffset     (.-endOffset hit)}))
       (group-into-phrases)
       (map (fn [group]
              (let [startOffset (:startOffset (apply min-key :startOffset group))
                    endOffset (:endOffset (apply max-key :endOffset group))
                    meta (.getMetadata (.getQuery monitor (.getQueryId match)))]
                {:text          (subs text startOffset endOffset)
                 :type          (or (get meta "_type") type-name)
                 :dict-entry-id (.getQueryId match)
                 :meta          (into {} meta)
                 :begin-offset  startOffset
                 :end-offset    endOffset})))))

(defn mark-text [^String text ^Monitor monitor ^String type-name]
  (->> (matches text monitor)
       (map #(match->annotation text monitor type-name %))
       (flatten)))

(defn escape-query-string [query-string]
  (-> query-string
      (s/replace #"([\"\\])" "\\\\\\\\\\\\$1")
      (s/replace #"([:\/+\~\-\|<>&\(\)\[\]=!{}\^\*\?])" "\\\\\\\\$1")
      (s/replace #"(\bAND\b)" "\\\\\\\\$1")
      (s/replace #"(\bOR\b)" "\\\\\\\\$1")
      (s/trim)))

(defn as-phrase-query [query-string] (str "\"" (escape-query-string query-string) "\""))

(defn prepare-synonyms [query-id {:keys [synonyms] :as dict-entry}]
  (map (fn [synonym]
         (-> dict-entry
             (assoc :text synonym)
             (dissoc :synonyms)
             (assoc :id (str (UUID/randomUUID)))
             (update-in [:meta] assoc :synonym? "true" :query-id query-id)))
       synonyms))

(defn save-queries-in-monitor [monitor monitor-queries]
  (doseq [portion (partition-all 4000 monitor-queries)]
    (try
      (.update monitor ^Iterable portion)
      (catch Exception e
        (.printStackTrace e)))))

(defn dict-entry->monitor-query [{:keys [id text meta type]} nr]
  (let [query-id (or id (str nr))
        metadata (reduce-kv (fn [m k v] (assoc m (name k) v)) {} (if type (assoc meta :_type type) meta))]
    (MonitorQuery. query-id (as-phrase-query text) metadata)))

(defn dict-entries->monitor-queries [dict-entries]
  (flatten
    (map (fn [{id :id :as dict-entry} nr]
           (let [query-id (or id (str nr))]
             (cons
               (dict-entry->monitor-query dict-entry nr)
               (map #(dict-entry->monitor-query % nil) (prepare-synonyms query-id dict-entry)))))
         dict-entries (range))))

(defn prepare-monitor [monitor dict-entries]
  (save-queries-in-monitor monitor (dict-entries->monitor-queries dict-entries)))

(defn create-monitor [analysis-conf]
  (Monitor.
    (proxy [MonitorQueryParser] []
      (parse [queryString metadata]
        (.parse (ComplexPhraseQueryParser.
                  (get-field-name analysis-conf)
                  (get-string-analyzer analysis-conf)) queryString)))
    (MatchAllPresearcher.)))

(defn get-dictionary-entries [groups analysis-conf]
  (get groups (conf->analyzers analysis-conf)))

(defn setup-monitors [dictionary]
  (let [^Monitor kw-monitor (create-monitor keyword-analysis-conf)
        ^Monitor lowercased-monitor (create-monitor lowercase-analysis-conf)
        ^Monitor ascii-folded-monitor (create-monitor ascii-folding-analysis-conf)
        ^Monitor lowercased-ascii-folded-monitor (create-monitor lowercase-ascii-fold-analysis-conf)

        groups (group-by conf->analyzers dictionary)

        _ (prepare-monitor kw-monitor (get-dictionary-entries groups keyword-analysis-conf))
        _ (prepare-monitor lowercased-monitor (get-dictionary-entries groups lowercase-analysis-conf))
        _ (prepare-monitor ascii-folded-monitor (get-dictionary-entries groups ascii-folding-analysis-conf))
        _ (prepare-monitor lowercased-ascii-folded-monitor (get-dictionary-entries groups lowercase-ascii-fold-analysis-conf))]
    [kw-monitor lowercased-monitor ascii-folded-monitor lowercased-ascii-folded-monitor]))

(defn synonym-annotation? [annotation]
  (= "true" (get-in annotation [:meta "synonym?"])))

(defn meta-type? [annotation]
  (string? (get-in annotation [:meta "_type"])))

(defn post-process [annotation]
  (cond-> annotation
          (synonym-annotation? annotation) (assoc :dict-entry-id (get-in annotation [:meta "query-id"]))
          (meta-type? annotation) (update-in [:meta] dissoc "_type")))

(defn annotator [dictionary & {:keys [type-name optimize-dictionary? validate-dictionary?]}]
  (when validate-dictionary? (sch/validate schema/Dictionary dictionary))
  (let [dictionary (if optimize-dictionary? (optimizer/optimize dictionary) dictionary)
        type-name (if (s/blank? type-name) "PHRASE" type-name)
        monitors (setup-monitors dictionary)]
    (fn [text & {:keys [merge-annotations?]}]
      (if (s/blank? text)
        []
        (let [annotations (map post-process
                               (flatten (pmap (fn [monitor] (mark-text text monitor type-name)) monitors)))]
          (if merge-annotations?
            (merger/merge-same-type-annotations annotations)
            annotations))))))
