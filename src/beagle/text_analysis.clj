(ns beagle.text-analysis
  (:import (org.apache.lucene.analysis Analyzer Analyzer$TokenStreamComponents Tokenizer)
           (org.apache.lucene.analysis.core LowerCaseFilter WhitespaceTokenizer)
           (org.apache.lucene.analysis.miscellaneous ASCIIFoldingFilter)
           (org.apache.lucene.analysis.standard ClassicFilter StandardTokenizer)
           (org.apache.lucene.analysis.tokenattributes CharTermAttribute)))

(def analysis-keys [:ascii-fold? :case-sensitive?])

(defn analyzers [tokenizer-kw]
  (let [^Tokenizer tokenizer (case tokenizer-kw
                               :standard (StandardTokenizer.)
                               :whitespace (WhitespaceTokenizer.)
                               (StandardTokenizer.))]
    {#{} {:analyzer   (proxy [Analyzer] []
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
                                :field-name "text.ascii-folded-lowercased"}}))

(def default-conf {:case-sensitive? true :ascii-fold? false})

(defn conf->analyzers [{:keys [ascii-fold? case-sensitive?]
                        :or   {ascii-fold?     (:ascii-fold? default-conf)
                               case-sensitive? (:case-sensitive? default-conf)}}]
  (cond-> #{}
          (false? case-sensitive?) (conj :lowercase)
          (true? ascii-fold?) (conj :ascii-fold)))

(defn ^Analyzer get-string-analyzer [analysis-conf text-analysis-resources]
  (get-in text-analysis-resources [(conf->analyzers analysis-conf) :analyzer]))

(defn ^String get-field-name [analysis-conf text-analysis-resources]
  (get-in text-analysis-resources [(conf->analyzers analysis-conf) :field-name]))

(defn text->token-strings
  "Given a text and an analyzer returns an array of tokens as strings."
  [^String text ^Analyzer analyzer]
  (let [token-stream (.tokenStream analyzer "not-important" text)
        ^CharTermAttribute termAtt (.addAttribute token-stream CharTermAttribute)]
    (.reset token-stream)
    (reduce (fn [acc _]
              (if (.incrementToken token-stream)
                (conj acc (.toString termAtt))
                (do
                  (.close token-stream)
                  (reduced acc)))) [] (range))))
