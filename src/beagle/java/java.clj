(ns beagle.java.java
  (:gen-class)
  (:require [beagle.phrases :as phrases]))

(gen-class
  :name lt.tokenmill.beagle.phrases.DictionaryEntry
  :state "state"
  :init "init"
  :constructors {[String] []}
  :methods [[text [] String]
            [type [] String]
            [setType [String] void]
            [id [] String]
            [setId [String] void]
            [synonyms [] java.util.Collection]
            [setSynonyms [java.util.Collection] void]
            [caseSensitive [] Boolean]
            [setCaseSensitive [Boolean] void]
            [asciiFold [] Boolean]
            [setAsciiFold [Boolean] void]
            [stem [] Boolean]
            [setStem [Boolean] void]
            [stemmer [] String]
            [setStemmer [String] void]
            [slop [] Integer]
            [setSlop [Integer] void]
            [meta [] java.util.Map]
            [setMeta [java.util.Map] void]]
  :prefix DictionaryEntry-)

(defn DictionaryEntry-init [phrase]
  [[] (atom {:text phrase})])

(defn DictionaryEntry-text [this]
  (@(.state this) :text))
(defn DictionaryEntry-type [this]
  (@(.state this) :type))
(defn DictionaryEntry-setType [this type]
  (swap! (.state this) assoc :type type))
(defn DictionaryEntry-id [this]
  (@(.state this) :id))
(defn DictionaryEntry-setId [this id]
  (swap! (.state this) assoc :id id))
(defn DictionaryEntry-synonyms [this]
  (@(.state this) :synonyms))
(defn DictionaryEntry-setSynonyms [this synonyms]
  (swap! (.state this) assoc :synonyms synonyms))
(defn DictionaryEntry-caseSensitive [this]
  (@(.state this) :case-sensitive?))
(defn DictionaryEntry-setCaseSensitive [this case-sensitive]
  (swap! (.state this) assoc :case-sensitive? case-sensitive))
(defn DictionaryEntry-asciiFold [this]
  (@(.state this) :ascii-fold?))
(defn DictionaryEntry-setAsciiFold [this ascii-fold]
  (swap! (.state this) assoc :ascii-fold? ascii-fold))
(defn DictionaryEntry-stem [this]
  (@(.state this) :stem?))
(defn DictionaryEntry-setStem [this stem]
  (swap! (.state this) assoc :stem? stem))
(defn DictionaryEntry-stemmer [this]
  (@(.state this) :stemmer))
(defn DictionaryEntry-setStemmer [this stemmer]
  (swap! (.state this) assoc :stemmer stemmer))
(defn DictionaryEntry-slop [this]
  (@(.state this) :slop))
(defn DictionaryEntry-setSlop [this slop]
  (swap! (.state this) assoc :slop slop))
(defn DictionaryEntry-meta [this]
  (@(.state this) :meta))
(defn DictionaryEntry-setMeta [this meta]
  (swap! (.state this) assoc :meta meta))

(gen-class
  :name lt.tokenmill.beagle.phrases.Annotator
  :state "state"
  :init "init"
  :constructors {[java.util.Collection]               []
                 [java.util.Collection java.util.Map] []}
  :prefix Phrases-
  :methods [[annotate [String] java.util.Collection]
            [annotate [String java.util.Map] java.util.Collection]])

(defn Phrases-init
  ([dictionary] (Phrases-init dictionary {}))
  ([dictionary opts]
   [[] (atom {:dictionary   dictionary
              :annotator-fn (phrases/highlighter
                              (map (fn [dictionary-entry]
                                     {:text            (.text dictionary-entry)
                                      :type            (.type dictionary-entry)
                                      :id              (.id dictionary-entry)
                                      :synonyms        (.synonyms dictionary-entry)
                                      :case-sensitive? (.caseSensitive dictionary-entry)
                                      :ascii-fold?     (.asciiFold dictionary-entry)
                                      :stem?           (.stem dictionary-entry)
                                      :stemmer         (keyword (.stemmer dictionary-entry))
                                      :slop            (.slop dictionary-entry)
                                      :meta            (.meta dictionary-entry)}) dictionary)
                              (reduce (fn [m [k v]]
                                        (assoc m (keyword k) v)) {} opts))})]))

(defn Phrases-annotate
  ([this text] (Phrases-annotate this text {}))
  ([this text opts]
   (map (fn [ann] (lt.tokenmill.beagle.phrases.Annotation.
                    (:text ann)
                    (:type ann)
                    (long (:begin-offset ann))
                    (long (:end-offset ann))
                    (:dict-entry-id ann)
                    (:meta ann)))
        ((@(.state this) :annotator-fn) text (reduce (fn [m [k v]]
                                                       (assoc m (keyword k) v)) {} opts)))))
