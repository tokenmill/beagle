(ns beagle.java-test
  (:require [clojure.test :refer [deftest is]]))

(deftest simple-java-interface
  (let [de (doto (lt.tokenmill.beagle.phrases.DictionaryEntry. "test")
             (.setSlop (Integer. 1)))
        annotator (lt.tokenmill.beagle.phrases.Annotator. [de] {})]
    (is (= "test" (first (map #(.text %) (.annotate annotator "test txt" {})))))))
