(ns beagle.annotation-merge-test
  (:require [clojure.test :refer [deftest is]]
            [beagle.phrases :as phrases]
            [beagle.annotation-merger :as merger]))

(deftest annotator-with-merge-option-test
  (let [dictionary [{:text "1 2"} {:text "2"} {:text "1 2 3 4"}
                    {:text "4"} {:text "5"} {:text "6 5 3 7"} {:text "6 5"}]
        highlighter-fn (phrases/highlighter dictionary {:type-name "TEST"})
        text "A B C 1 2 3 4 D E F G 6 5 3 7"]
    (is (= (count (highlighter-fn text {:merge-annotations? false})) (count (highlighter-fn text))))
    (is (< (count (highlighter-fn text {:merge-annotations? true})) (count (highlighter-fn text))))
    (is (= [(set (vals {:begin-offset  6
                        :dict-entry-id "2"
                        :end-offset    13
                        :meta          {}
                        :text          "1 2 3 4"
                        :type          "TEST"}))
            (set (vals {:begin-offset  22
                        :dict-entry-id "5"
                        :end-offset    29
                        :meta          {}
                        :text          "6 5 3 7"
                        :type          "TEST"}))]
           (map #(-> % vals set) (highlighter-fn text {:merge-annotations? true}))))))

(deftest annotation-merge-test
  (is (= [{:text "AAAAA" :type "TEST" :dict-entry-id "1" :meta {} :begin-offset 0 :end-offset 5}]
         (merger/merge-same-type-annotations
           [{:text "AAAAA" :type "TEST" :dict-entry-id "1" :meta {} :begin-offset 0 :end-offset 5}
            {:text "A" :type "TEST" :dict-entry-id "3" :meta {} :begin-offset 0 :end-offset 1}
            {:text "AAAA" :type "TEST" :dict-entry-id "2" :meta {} :begin-offset 1 :end-offset 5}])))

  (is (= [{:text "AAAAA" :type "TEST" :dict-entry-id "1" :meta {} :begin-offset 0 :end-offset 5}
          {:text "AAA" :type "TEST2" :dict-entry-id "10" :meta {} :begin-offset 0 :end-offset 3}]
         (merger/merge-same-type-annotations
           [{:text "AAAAA" :type "TEST" :dict-entry-id "1" :meta {} :begin-offset 0 :end-offset 5}
            {:text "A" :type "TEST" :dict-entry-id "2" :meta {} :begin-offset 0 :end-offset 1}
            {:text "AAAA" :type "TEST" :dict-entry-id "3" :meta {} :begin-offset 1 :end-offset 5}
            {:text "AAA" :type "TEST2" :dict-entry-id "10" :meta {} :begin-offset 0 :end-offset 3}
            {:text "A" :type "TEST2" :dict-entry-id "11" :meta {} :begin-offset 0 :end-offset 1}]))))

