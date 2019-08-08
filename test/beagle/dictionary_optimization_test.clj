(ns beagle.dictionary-optimization-test
  (:require [clojure.test :refer :all]
            [beagle.dictionary-optimizer :as optimizer]
            [beagle.phrases :as annotations]))

(deftest meta-merge-test
  (is (optimizer/mergeable-meta? nil {:meta {:email "123"}}))
  (is (optimizer/mergeable-meta? {:meta {}} {:meta {:email "123"}}))
  (is (optimizer/mergeable-meta? {:meta {:email "123"}} nil))
  (is (optimizer/mergeable-meta? {:meta {:email "123"}} {:meta {:email "123"}}))
  (is (optimizer/mergeable-meta? {:meta {:email "123"}} {:meta {:email "123" :total 5646}}))
  (is (optimizer/mergeable-meta? {:meta {:email "123" :total 5646}} {:meta {:email "123"}}))
  (is (not (optimizer/mergeable-meta? {:meta {:email "123"}} {:meta {:email "321"}})))
  (is (not (optimizer/mergeable-meta? {:meta {:email "123" :total 5646}} {:meta {:email "123" :total 9999}})))
  (is (= [{:ascii-fold?     true
           :case-sensitive? true
           :id              "test-id"
           :meta            {:abc   "123" :email "test@example.com"}
           :synonyms        ["abc" "XXXX"]
           :text            "test text"}
          {:ascii-fold?     true
           :case-sensitive? true
           :id              "test-id"
           :meta            {:email "bobby@example.com"}
           :synonyms        ["def"]
           :text            "test text"}]
         (optimizer/aggregate-entries-by-meta
           [{:text            "test text"
             :id              "test-id"
             :synonyms        ["abc"]
             :case-sensitive? true
             :ascii-fold?     true
             :meta            {:email "test@example.com"}}
            {:text            "test text"
             :id              "test-id"
             :synonyms        ["def"]
             :case-sensitive? true
             :ascii-fold?     true
             :meta            {:email "bobby@example.com"}}
            {:text            "test text"
             :id              "test-id"
             :synonyms        ["XXXX"]
             :case-sensitive? true
             :ascii-fold?     true
             :meta            {:email "test@example.com" :abc "123"}}]))))

(deftest dictionary-optimization-test
  (let [dictionary [{:case-sensitive? true
                     :ascii-fold?     true
                     :synonyms        ["AAAA1"]
                     :text            "AAAA"}
                    {:case-sensitive? true
                     :ascii-fold?     true
                     :synonyms        ["AAAA2"]
                     :text            "AAAA"}
                    {:case-sensitive? false
                     :ascii-fold?     true
                     :synonyms        ["AAAA3"]
                     :text            "AAAA"}
                    {:case-sensitive? true
                     :ascii-fold?     true
                     :synonyms        ["AAAA4"]
                     :text            "AAAA"}
                    {:case-sensitive? true
                     :ascii-fold?     false
                     :synonyms        ["AAAA5"]
                     :text            "AAAA"}
                    {:case-sensitive? true
                     :ascii-fold?     false
                     :synonyms        ["AAAA"]
                     :text            "AAAA"}
                    {:case-sensitive? false
                     :synonyms        ["BBBB1"]
                     :text            "BBBB"}
                    {:case-sensitive? false
                     :synonyms        ["BBBB"]
                     :text            "BBBB"}]
        expected-dictionary [{:text            "AAAA"
                              :synonyms        ["AAAA4" "AAAA2" "AAAA1"]
                              :case-sensitive? true
                              :ascii-fold?     true}
                             {:case-sensitive? false :ascii-fold? true :synonyms ["AAAA3"] :text "AAAA"}
                             {:text "AAAA" :synonyms ["AAAA5"] :case-sensitive? true :ascii-fold? false}
                             {:text "BBBB" :synonyms ["BBBB1"] :case-sensitive? false}]
        optimized-dictionary (optimizer/optimize dictionary)]
    (is (< (count optimized-dictionary) (count dictionary)))
    (is (= (count expected-dictionary) (count optimized-dictionary)))
    (is (= (set (map #(update % :synonyms set) expected-dictionary))
           (set (map #(update % :synonyms set) optimized-dictionary))))))

(deftest synonym-optimization
  (let [dictionary [{:text "test" :id "1" :synonyms ["beagle" "luwak1"]}]
        monitor-queries (annotations/dict-entries->monitor-queries dictionary :whitespace)]
    (is (= 3 (count monitor-queries)))
    (let [annotator (annotations/annotator dictionary :type-name "TEST")
          anns (annotator "this is a beagle text test luwak1")]
      (is (= 3 (count anns))))))
