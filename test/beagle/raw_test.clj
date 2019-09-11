(ns beagle.raw-test
  (:require [clojure.test :refer [deftest is]]
            [beagle.raw :as raw]))

(deftest smoke
  (let [txt "some text this other that"
        dictionary [{:text "this AND that" :id "1" :slop 1}]
        annotator-fn (raw/annotator dictionary)
        [ann1 :as anns] (annotator-fn txt {})]
    (prn anns)
    (is (= 1 (count anns)))
    (is (= "1" (:dict-entry-id ann1)))))
