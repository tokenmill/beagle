(ns beagle.java
  (:gen-class)
  (:require [beagle.phrases :as phrases]))

(gen-class
  :name   lt.tokenmill.beagle.phrases.DictionaryEntry
  :state "state"
  :init "init"
  :constructors {[String] []}
  :methods [[text [] String]]
  :prefix DictionaryEntry-)

(defn DictionaryEntry-init [phrase]
  [[] (atom {:text phrase})])

(defn DictionaryEntry-text [this]
  (@(.state this) :text))


(gen-class
  :name lt.tokenmill.beagle.phrases.Annotation
  :prefix Annotation-
  :state "state"
  :init "init"
  :constructors {[String String Long Long] []}
  :methods [[text [] String]
            [type [] String]
            [beginOffset [] Long]
            [endOffset [] Long]]
  :prefix Annotation-)

(defn Annotation-init [text type begin end]
  [[] (atom {:text text
             :type type
             :begin begin
             :end end})])

(defn Annotation-text [this]
  (@(.state this) :text))
(defn Annotation-type [this]
  (@(.state this) :type))
(defn Annotation-beginOffset [this]
  (@(.state this) :begin))
(defn Annotation-endOffset [this]
  (@(.state this) :end))

(gen-class
  :name lt.tokenmill.beagle.phrases.Annotator
  :state "state"
  :init "init"
  :constructors {[java.util.Collection] []
                 [String] []}
  :prefix Phrases-
  :methods [[annotate [String] java.util.Collection]])

(defn Phrases-init [dictionary]
  [[] (atom {:dictionary   dictionary
             :annotator-fn (phrases/highlighter
                             (map (fn [^lt.tokenmill.beagle.phrases.DictionaryEntry dictionary-entry]
                                    {:text (.text dictionary-entry)}) dictionary))})])

(defn Phrases-annotate [this text]
  (map (fn [ann] (lt.tokenmill.beagle.phrases.Annotation.
                   (:text ann)
                   (:type ann)
                   (long (:begin-offset ann))
                   (long (:end-offset ann))))
       ((@(.state this) :annotator-fn) text)))
