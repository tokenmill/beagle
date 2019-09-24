(ns beagle.lucene-alpha-test
  (:require [clojure.test :refer [deftest is]]
            [beagle.lucene-alpha :as lucene]))

(deftest smoke
  (let [txt "some text this other that"
        dictionary [{:text "this AND that" :id "1" :slop 1}]
        annotator-fn (lucene/annotator dictionary)
        [ann1 :as anns] (annotator-fn txt {})
        anns2 (annotator-fn txt)]
    (is (= anns anns2))
    (is (= 1 (count anns)))
    (is (= "1" (:dict-entry-id ann1)))))

(deftest smoke-2
  (let [txt "some text this AND"
        dictionary [{:text "this AND" :id "1" :slop 1}]
        annotator-fn (lucene/annotator dictionary)
        [ann1 :as anns] (annotator-fn txt)]
    (is (= 0 (count anns)))
    (is (nil? (:dict-entry-id ann1)))))

(deftest smoke-3
  (let [txt "some number 1234 test"
        dictionary [{:text "/.*\\d*.*/" :id "1" :slop 1}]
        annotator-fn (lucene/annotator dictionary)
        anns (annotator-fn txt)]
    (is (< 0 (count anns)))))
