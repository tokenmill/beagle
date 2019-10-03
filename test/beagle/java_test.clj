(ns beagle.java-test
  (:require [clojure.test :refer [deftest is]]))

(deftest simple-java-interface
  (let [de (doto (lt.tokenmill.beagle.phrases.DictionaryEntry. "test")
             (.setSlop (Integer. 1)))
        annotator (lt.tokenmill.beagle.phrases.Annotator. [de] {})]
    (is (= "test" (first (map #(.text %) (.annotate annotator "test txt" {})))))))

(deftest case-sensitivity
  (let [de (doto (lt.tokenmill.beagle.phrases.DictionaryEntry. "LYNDON BAINES JOHNSON")
             (.setCaseSensitive false))
        annotator (lt.tokenmill.beagle.phrases.Annotator. [de] {})]
    (is (= 1 (count (filter #(= "Lyndon Baines Johnson" (.text %)) (.annotate annotator "Lyndon Baines Johnson (/ˈlɪndən ˈbeɪnz/; August 27, 1908 – January 22, 1973), often referred to as LBJ, was an American politician who served as the 36th president of the United States from 1963 to 1969." {})))))))
