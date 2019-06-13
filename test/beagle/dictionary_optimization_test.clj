(ns beagle.dictionary-optimization-test
  (:require [clojure.test :refer :all]
            [beagle.dictionary-optimizer :as optimizer]
            [beagle.phrases :as annotations]
            [cheshire.core :as json]
            [clojure.java.io :as io]))

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
        monitor-queries (annotations/dict-entries->monitor-queries dictionary)]
    (is (= 3 (count monitor-queries)))
    (let [annotator (annotations/annotator dictionary :type-name "TEST")
          anns (annotator "this is a beagle text test luwak1")]
      (is (= 3 (count anns))))))

(deftest optimization-log
  (let [suggestions (optimizer/dry-run [{:text "test" :id "1" :synonyms ["beagle" "luwak1"]}
                                        {:text "test" :id "2" :synonyms ["beagle" "luwak1"]}
                                        {:text "test" :id "3" :synonyms ["test" "luwak2"]}
                                        {:text "test" :id "4" :synonyms ["luwak222"]}
                                        {:text "test" :id "5" :synonyms ["beagle" "luwak1"] :case-sensitive? true}
                                        {:text "test" :id "6" :synonyms ["beagle" "luwak1"] :ascii-fold? true}
                                        {:text "test" :id "7" :synonyms ["beagle"] :ascii-fold? true}])]
    (is (= [{:suggestion "dictionary item '0' and '1' are identical"
             :dictionary-items [{:text "test" :id "1" :synonyms ["beagle" "luwak1"] :entry-id 0}
                                {:text "test" :id "2" :synonyms ["beagle" "luwak1"] :entry-id 1}]}
            {:suggestion "dictionary item '2' has synonym equal to its text"
             :dictionary-items [{:text "test" :id "3" :synonyms ["test" "luwak2"] :entry-id 2}]}
            {:suggestion "dictionary item '0' and '3' differ only by synonyms list - mergeable"
             :dictionary-items [{:text "test" :entry-id 0 :synonyms ["beagle" "luwak2" "luwak1"] :id "1"}
                                {:text "test" :id "4" :synonyms ["luwak222"] :entry-id 3}]}
            {:suggestion "dictionary item '5' synonyms are superset of item '6' synonyms list - mergeable"
             :dictionary-items [{:text "test" :id "6" :synonyms ["beagle" "luwak1"] :ascii-fold? true :entry-id 5}
                                {:text "test" :id "7" :synonyms ["beagle"] :ascii-fold? true :entry-id 6}]}
            {:suggestion "dictionary item '6' synonyms are superset of item '5' synonyms list - mergeable"
             :dictionary-items [{:text "test" :id "7" :synonyms ["beagle"] :ascii-fold? true :entry-id 6}
                                {:text "test" :id "6" :synonyms ["beagle" "luwak1"] :ascii-fold? true :entry-id 5}]}]
           suggestions))))
