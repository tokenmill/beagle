(ns beagle.optimization-suggestions-test
  (:require [clojure.test :refer :all]
            [beagle.dictionary-optimizer :as optimizer]))

(deftest optimization-suggestions
  (testing "Suggestions for similar dictionary items"
    (is (= [{:dictionary-items [{:id "1" :synonyms ["beagle"] :text "test"} {:id "2" :synonyms ["luwak1"] :text "test"}]
             :suggestion       "Dictionary items '[1 2]' have identical `[text case-sensitivity ascii-folding] features."}]
           (optimizer/dry-run [{:text "test" :id "1" :synonyms ["beagle"]}
                               {:text "test" :id "2" :synonyms ["luwak1"]}]))))

  (testing "Suggestions for two similar dictionary item groups"
    (is (= [{:suggestion       "Dictionary items '[1 3]' have identical `[text case-sensitivity ascii-folding] features."
             :dictionary-items [{:id "1" :synonyms ["beagle"] :text "test"} {:id "3" :synonyms ["beagle"] :text "test"}]}
            {:suggestion       "Dictionary items '[2 4]' have identical `[text case-sensitivity ascii-folding] features."
             :dictionary-items [{:id "2" :synonyms ["luwak2"] :text "test2"} {:id "4" :synonyms ["beagle3"] :text "test2"}]}]
           (optimizer/dry-run [{:id "1" :synonyms ["beagle"] :text "test"}
                               {:id "2" :synonyms ["luwak2"] :text "test2"}
                               {:id "3" :synonyms ["beagle"] :text "test"}
                               {:id "4" :synonyms ["beagle3"] :text "test2"}]))))

  (testing "Suggestions for single dictionary item"
    (is (= [] (optimizer/dry-run [{:id "1" :synonyms ["beagle"] :text "test"}]))))

  (testing "Suggestions for distinct dictionary items"
    (is (= [] (optimizer/dry-run [{:id "1" :case-sensitive? true :synonyms ["beagle"] :text "test"}
                                  {:id "2" :synonyms ["beagle"] :text "test2"}
                                  {:id "3" :ascii-fold? false :synonyms ["beagle"] :text "test3"}]))))

  (testing "Suggestions for two similar dictionary item groups and one distinct dictionary item"
    (is (= [{:suggestion       "Dictionary items '[test 3 4]' have identical `[text case-sensitivity ascii-folding] features."
             :dictionary-items [{:synonyms ["beagle"] :text "test"}
                                {:id "3" :synonyms ["beagle"] :text "test"}
                                {:id "4" :synonyms ["luwak222"] :text "test"}]}
            {:suggestion       "Dictionary items '[2 test2]' have identical `[text case-sensitivity ascii-folding] features."
             :dictionary-items [{:id "2" :synonyms ["luwak2"] :text "test2"} {:synonyms ["beagle3"] :text "test2"}]}]
           (optimizer/dry-run [{:synonyms ["beagle"] :text "test"}
                               {:id "2" :synonyms ["luwak2"] :text "test2"}
                               {:id "3" :synonyms ["beagle"] :text "test"}
                               {:id "4" :synonyms ["luwak222"] :text "test"}
                               {:synonyms ["beagle3"] :text "test2"}
                               {:synonyms ["beagle"] :text "test" :ascii-fold? true}])))))
