(ns beagle.java-test
  (:require [clojure.test :refer :all]))

(deftest simple-java-interface
  (let [de (lt.tokenmill.beagle.phrases.DictionaryEntry. "test")
        annotator (lt.tokenmill.beagle.phrases.Annotator. [de] {"a" "a"})]
    (is (= "test" (first (map #(.text %) (.annotate annotator "test txt" {})))))))
