(ns beagle.text-analysis-test
  (:require [clojure.test :refer [deftest is]]
            [beagle.text-analysis :as text-analysis]))

(deftest field-name-construction
  (is (= "text.standard-tokenizer"
         (text-analysis/get-field-name {} {})))
  (is (= "text.standard-tokenizer"
         (text-analysis/get-field-name {:case-sensitive? true} {})))
  (is (= "text.standard-tokenizer.lowercased"
         (text-analysis/get-field-name {:case-sensitive? false} {})))
  (is (= "text.standard-tokenizer.ascii-folded"
         (text-analysis/get-field-name {:ascii-fold? true} {})))
  (is (= "text.standard-tokenizer.stemmed-english"
         (text-analysis/get-field-name {:stem? true} {})))
  (is (= "text.standard-tokenizer.stemmed-lithuanian"
         (text-analysis/get-field-name {:stem? true :stemmer :lithuanian} {})))
  (is (= "text.standard-tokenizer.ascii-folded-lowercased-stemmed-lithuanian"
         (text-analysis/get-field-name {:ascii-fold? true
                                        :case-sensitive? false
                                        :stem? true
                                        :stemmer :lithuanian} {}))))

(deftest token-stream
  (let [txt "These are tests."]
    (is (= ["These" "are" "tests"]
           (text-analysis/text->token-strings
             txt (text-analysis/get-string-analyzer {:case-sensitive? true} {}))))
    (is (= ["these" "are" "tests"]
           (text-analysis/text->token-strings
             txt (text-analysis/get-string-analyzer {:case-sensitive? false} {}))))
    (is (= ["these" "are" "tests"]
           (text-analysis/text->token-strings
             txt (text-analysis/get-string-analyzer {:case-sensitive? false
                                                     :ascii-fold? true} {}))))
    (is (= ["these" "are" "test"]
           (text-analysis/text->token-strings
             txt (text-analysis/get-string-analyzer {:case-sensitive? false
                                                     :ascii-fold? true
                                                     :stem? true} {}))))
    ; this one is surprising but correct
    (is (= ["these" "are" "tests."]
           (text-analysis/text->token-strings
             txt (text-analysis/get-string-analyzer {:case-sensitive? false
                                                     :ascii-fold? true
                                                     :stem? true} {:tokenizer :whitespace}))))))
