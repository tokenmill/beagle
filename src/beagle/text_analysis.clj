(ns beagle.text-analysis
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log])
  (:import (org.apache.lucene.analysis Analyzer Analyzer$TokenStreamComponents Tokenizer TokenFilter TokenStream)
           (org.apache.lucene.analysis.core LowerCaseFilter WhitespaceTokenizer LetterTokenizer KeywordTokenizer UnicodeWhitespaceTokenizer)
           (org.apache.lucene.analysis.miscellaneous ASCIIFoldingFilter)
           (org.apache.lucene.analysis.standard ClassicFilter StandardTokenizer)
           (org.apache.lucene.analysis.tokenattributes CharTermAttribute)
           (org.apache.lucene.analysis.snowball SnowballFilter)
           (org.tartarus.snowball.ext LithuanianStemmer ArabicStemmer ArmenianStemmer BasqueStemmer EnglishStemmer CatalanStemmer DanishStemmer DutchStemmer EstonianStemmer FinnishStemmer FrenchStemmer German2Stemmer GermanStemmer HungarianStemmer IrishStemmer ItalianStemmer KpStemmer LovinsStemmer NorwegianStemmer PorterStemmer PortugueseStemmer RomanianStemmer RussianStemmer SpanishStemmer SwedishStemmer TurkishStemmer)
           (org.tartarus.snowball SnowballProgram)))

(defn ^SnowballProgram stemmer
  "Creates a stemmer object given the stemmer keyword.
  Default stemmer is English."
  [stemmer-kw]
  (case stemmer-kw
    :arabic (ArabicStemmer.)
    :armenian (ArmenianStemmer.)
    :basque (BasqueStemmer.)
    :catalan (CatalanStemmer.)
    :danish (DanishStemmer.)
    :dutch (DutchStemmer.)
    :english (EnglishStemmer.)
    :estonian (EstonianStemmer.)
    :finnish (FinnishStemmer.)
    :french (FrenchStemmer.)
    :german2 (German2Stemmer.)
    :german (GermanStemmer.)
    :hungarian (HungarianStemmer.)
    :irish (IrishStemmer.)
    :italian (ItalianStemmer.)
    :kp (KpStemmer.)
    :lithuanian (LithuanianStemmer.)
    :lovins (LovinsStemmer.)
    :norwegian (NorwegianStemmer.)
    :porter (PorterStemmer.)
    :portuguese (PortugueseStemmer.)
    :romanian (RomanianStemmer.)
    :russian (RussianStemmer.)
    :spanish (SpanishStemmer.)
    :swedish (SwedishStemmer.)
    :turkish (TurkishStemmer.)
    (do
      (when stemmer-kw
        (log/debugf "Stemmer '%s' not found! EnglishStemmer is used." stemmer-kw))
      (EnglishStemmer.))))

(defn tokenizer [tokenizer-kw]
  (case tokenizer-kw
    :keyword (KeywordTokenizer.)
    :letter (LetterTokenizer.)
    :standard (StandardTokenizer.)
    :unicode-whitespace (UnicodeWhitespaceTokenizer.)
    :whitespace (WhitespaceTokenizer.)
    (do
      (when tokenizer-kw
        (log/debugf "Tokenizer '%s' not found. StandardTokenizer is used." tokenizer-kw))
      (StandardTokenizer.))))

(defn analyzer-constructor [{tokenizer-kw    :tokenizer
                             ascii-fold?     :ascii-fold?
                             case-sensitive? :case-sensitive?
                             stem?           :stem?
                             stemmer-kw      :stemmer}]
  (let [^Tokenizer tokenizr (tokenizer tokenizer-kw)
        filters-chain (cond-> tokenizr
                              (not case-sensitive?) (LowerCaseFilter.)
                              ascii-fold? (ASCIIFoldingFilter.)
                              stem? (SnowballFilter. (stemmer stemmer-kw)))]
    (proxy [Analyzer] []
      (createComponents [^String field-name]
        (Analyzer$TokenStreamComponents.
          tokenizr ^TokenFilter (if (instance? Tokenizer filters-chain)
                                  (ClassicFilter. tokenizr)
                                  filters-chain))))))

(defn field-name-constructor [{tokenizer-kw    :tokenizer
                               ascii-fold?     :ascii-fold?
                               case-sensitive? :case-sensitive?
                               stem?           :stem?
                               stemmer-kw      :stemmer}]
  (let [tokenizr (str (name (or tokenizer-kw :standard)) "-tokenizer")
        filters (cond-> []
                        (not case-sensitive?) (conj "lowercased")
                        ascii-fold? (conj "ascii-folded")
                        stem? (conj (str "stemmed-" (name (or stemmer-kw :english)))))]
    (if (seq filters)
      (str "text" "." tokenizr "." (string/join "-" (sort filters)))
      (str "text" "." tokenizr))))

(def analyzer (memoize analyzer-constructor))
(def field-name (memoize field-name-constructor))

(def default-conf
  {:tokenizer       :standard
   :case-sensitive? true
   :ascii-fold?     false
   :stem?           false
   :stemmer         :english})

(def conf-fields (keys default-conf))

(defn ^Analyzer get-string-analyzer [analysis-conf default-analysis-conf]
  (analyzer (select-keys (merge default-conf default-analysis-conf analysis-conf)
                         conf-fields)))

(defn ^String get-field-name [analysis-conf default-analysis-conf]
  (field-name (select-keys (merge default-conf default-analysis-conf analysis-conf)
                           conf-fields)))

(defn text->token-strings
  "Given a text and an analyzer returns a list of tokens as strings."
  [^String text ^Analyzer analyzer]
  (let [^TokenStream token-stream (.tokenStream analyzer "not-important" text)
        ^CharTermAttribute termAtt (.addAttribute token-stream CharTermAttribute)]
    (.reset token-stream)
    (reduce (fn [acc _]
              (if (.incrementToken token-stream)
                (conj acc (.toString termAtt))
                (do
                  (.end token-stream)
                  (.close token-stream)
                  (reduced acc)))) [] (range))))
