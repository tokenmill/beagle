(ns beagle.phrases-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [beagle.phrases :as phrases]
            [beagle.schema :as schema]))

(s/def ::opts (s/* (s/cat :opt keyword? :val any?)))

(s/fdef phrases/highlighter
        :args (s/alt :unary (s/cat :dictionary ::schema/dictionary)
                     :binary (s/cat :dictionary ::schema/dictionary :opts any?))
        :ret (s/fspec :args (s/alt :unary (s/cat :text string?)
                                   :binary (s/cat :text string? :opts any?))
                      :ret ::schema/annotations))

(stest/instrument `phrases/highlighter)

(s/exercise-fn `phrases/highlighter)

(def label "LABEL")

(deftest dictionary-entry-record
  (let [dictionary [(schema/map->DictionaryEntry {:text "test"})]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated test phrase after annotated")]
    (is (= 1 (count anns)))))

(deftest type-per-dictionary-entry
  (let [dictionary [{:text "test phrase" :id "1" :meta {:test "test"} :type "CUSTOM"}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated test phrase after annotated")]
    (is (seq (s/conform ::schema/annotations anns)))
    (is (seq anns))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "CUSTOM" (-> anns first :type)))
    (is (= "test phrase" (-> anns first :text)))
    (is (nil? (-> anns first (get-in [:meta "_type"]))))))

(deftest id
  (let [dictionary [{:text "test" :id "1" :meta {:test "test"}}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated test after annotated")]
    (is (seq anns))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "LABEL" (-> anns first :type)))))

(deftest metadata-append
  (let [dictionary [{:text "test" :meta {"email" "test@example.com"}}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated test after annotated")]
    (is (seq anns))
    (is (= {"email" "test@example.com"} (-> anns first :meta)))))

(deftest case-sensitivity
  (testing "case sensitive"
    (let [dictionary [{:text "test"}]
          highlighter-fn (phrases/highlighter dictionary {:type-name label})
          anns (highlighter-fn "before annotated test after annotated")]
      (is (seq anns)))
    (let [dictionary [{:text "TeSt" :case-sensitive? true}]
          highlighter-fn (phrases/highlighter dictionary {:type-name label})
          anns (highlighter-fn "before annotated test after annotated")]
      (is (empty? anns)))
    (let [label "LABEL"
          dictionary [{:text "test" :case-sensitive? true}]
          highlighter-fn (phrases/highlighter dictionary {:type-name label})
          anns (highlighter-fn "before annotated Test after annotated")]
      (is (empty? anns))))

  (testing "case insensitive"
    (let [dictionary [{:text "TeSt" :case-sensitive? false}]
          highlighter-fn (phrases/highlighter dictionary {:type-name label})
          anns (highlighter-fn "before annotated test after annotated")]
      (is (seq anns)))
    (let [dictionary [{:text "test" :case-sensitive? false}]
          highlighter-fn (phrases/highlighter dictionary {:type-name label})
          anns (highlighter-fn "before annotated test after annotated")]
      (is (seq anns)))))

(deftest ascii-folding-dictionary
  (let [dictionary [{:text "wörd"}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated wörd after annotated")]
    (is (seq anns)))
  (let [dictionary [{:text "wörd"}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated word after annotated")]
    (is (empty? anns)))
  (let [label "LABEL"
        dictionary [{:text "wörd" :ascii-fold? true}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated word after annotated")]
    (is (seq anns)))
  (let [dictionary [{:text "word" :ascii-fold? true}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated wörd after annotated")]
    (is (seq anns)))
  (let [label "LABEL"
        dictionary [{:text "word" :ascii-fold? false}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated wörd after annotated")]
    (is (empty? anns))))

(deftest ascii-folding-with-case-sensitivity
  (let [label "TYPE"]
    (testing "case sensitive"
      (let [dictionary [{:text "schön" :ascii-fold? true}]
            highlighter-fn (phrases/highlighter dictionary {:type-name label})
            anns (highlighter-fn "before annotated Schön after annotated")]
        (is (empty? anns)))
      (let [dictionary [{:text "Schön" :ascii-fold? true}]
            highlighter-fn (phrases/highlighter dictionary {:type-name label})
            anns (highlighter-fn "before annotated Schon after annotated")]
        (is (seq anns)))
      (let [dictionary [{:text "schön" :ascii-fold? true}]
            highlighter-fn (phrases/highlighter dictionary {:type-name label})
            anns (highlighter-fn "before annotated Schon after annotated")]
        (is (empty? anns))))

    (testing "case insensitive"
      (let [dictionary [{:text "schön" :ascii-fold? true :case-sensitive? false}]
            highlighter-fn (phrases/highlighter dictionary {:type-name label})
            anns (highlighter-fn "before annotated Schon after annotated")]
        (is (seq anns))))
    (let [dictionary [{:text "schön" :ascii-fold? true :case-sensitive? false}]
          highlighter-fn (phrases/highlighter dictionary {:type-name label})
          anns (highlighter-fn "before annotated schon after annotated")]
      (is (seq anns)))
    (let [dictionary [{:text "schon" :ascii-fold? true :case-sensitive? false}]
          highlighter-fn (phrases/highlighter dictionary {:type-name label})
          anns (highlighter-fn "before annotated schön after annotated")]
      (is (seq anns)))

    (testing "false ascii fold"
      (let [dictionary [{:text "schon" :ascii-fold? false}]
            highlighter-fn (phrases/highlighter dictionary {:type-name label})
            anns (highlighter-fn "before annotated schön after annotated")]
        (is (empty? anns))))))

(deftest synonyms
  (let [dictionary [{:text "test" :id "1" :synonyms ["beagle"]}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated beagle after annotated")]
    (is (seq anns))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "beagle" (-> anns first :text))))

  (let [dictionary [{:text "test" :id "1" :synonyms ["Luwak"] :case-sensitive? true}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated beagle after annotated")]
    (is (empty? anns)))

  (let [dictionary [{:text "test" :id "1" :synonyms ["beagle"] :case-sensitive? false}]
        highlighter-fn (phrases/highlighter dictionary {:type-name label})
        anns (highlighter-fn "before annotated beagle after annotated")]
    (is (seq anns))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "beagle" (-> anns first :text))))

  (testing "synonyms with false ascii fold"
    (let [dictionary [{:text "test" :synonyms ["schön"] :ascii-fold? false}]
          highlighter-fn (phrases/highlighter dictionary {:type-name label})
          anns (highlighter-fn "before annotated schon after annotated")]
      (is (empty? anns)))
    (let [dictionary [{:text "test" :synonyms ["schön"] :ascii-fold? true}]
          highlighter-fn (phrases/highlighter dictionary {:type-name label})
          anns (highlighter-fn "before annotated schon after annotated")]
      (is (seq anns))
      (is (= "schon" (-> anns first :text))))))

(deftest phrase-end-sentence
  (let [dictionary [{:text "test-test"}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn "before annotated test-test.")]
    (is (seq anns))
    (is (= "test-test" (:text (first anns))))))

(deftest phrase-in-quotes
  (let [dictionary [{:text "test-test" :case-sensitive? false}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn "before annotated \"TEST-test\".")]
    (is (seq anns))
    (is (= "TEST-test" (:text (first anns))))))

(deftest phrase-in-quotes-should-not-match
  (let [dictionary [{:text "test-test" :case-sensitive? false}]
        highlighter-fn (phrases/highlighter dictionary {:tokenizer :whitespace})
        anns (highlighter-fn "before annotated \"TEST-test\".")]
    (is (empty? anns))))

(deftest overlapping-phrases
  (let [dictionary [{:text "test phrase test" :case-sensitive? false}]
        highlighter-fn (phrases/highlighter dictionary {:tokenizer :whitespace})
        anns (highlighter-fn "start test phrase test phrase test end")]
    (is (= 2 (count anns)))))

(deftest lt-stemming
  (let [dictionary [{:text "Kaunas" :id "1" :stem? true :stemmer :lithuanian}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn "Kauno miestas")]
    (is (seq anns))
    (is (= "Kauno" (-> anns first :text))))
  (let [dictionary [{:text "Kaunas Vilnius" :id "1" :stem? true}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn "Kaunas, Vilnius")]
    (is (seq anns))
    (is (= "Kaunas, Vilnius" (-> anns first :text))))
  (let [dictionary [{:text "Kaunas" :id "1" :case-sensitive? false :stem? true :stemmer :lithuanian}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn "kauno miestas")]
    (is (seq anns))
    (is (= "kauno" (-> anns first :text)))))

(deftest en-stemming
  (let [txt "who let the dogs out?"]
    (let [dictionary [{:text "dog" :id "1"}]
          highlighter-fn (phrases/highlighter dictionary)
          anns (highlighter-fn txt)]
      (is (empty? anns)))
    (let [dictionary [{:text "dog" :id "1" :stem? true}]
          highlighter-fn (phrases/highlighter dictionary)
          anns (highlighter-fn txt)]
      (is (seq anns))
      (is (= "dogs" (-> anns first :text))))
    (let [dictionary [{:text "dog" :id "1" :stem? true :stemmer :english}]
          highlighter-fn (phrases/highlighter dictionary)
          anns (highlighter-fn txt)]
      (is (seq anns))
      (is (= "dogs" (-> anns first :text))))
    (let [dictionary [{:text "dog" :id "1" :stem? true :stemmer :estonian}]
          highlighter-fn (phrases/highlighter dictionary)
          anns (highlighter-fn txt)]
      (is (empty? anns)))))

(deftest mixed-stemmers
  (let [txt "Saboniai plays basketball"
        dictionary [{:text "Sabonis" :id "1" :stem? true :stemmer :lithuanian}
                    {:text "play" :id "2" :stem? true :stemmer :english}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn txt)]
    (is (= 2 (count anns)))))

(deftest phrase-slop
  (let [txt "before start and end after"
        dictionary [{:text "start end" :id "1" :slop 1}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn txt)]
    (is (= 1 (count anns)))
    (is (= "start and end" (:text (first anns)))))
  (testing "all terms in the phrase should match"
    (let [txt "before start end after"
          dictionary [{:text "start NOPE end" :id "1" :slop 10}]
          highlighter-fn (phrases/highlighter dictionary)
          anns (highlighter-fn txt)]
      (is (empty? anns))))
  (let [txt "before start phrase and end phrase after"
        dictionary [{:text "start phrase end phrase" :id "1" :slop 1}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn txt)]
    (is (= 1 (count anns)))
    (is (= "start phrase and end phrase" (:text (first anns)))))
  (testing "phrase edit distance"
    (let [txt "before start end after"
          dictionary [{:text "end start" :id "1" :slop 0}]
          highlighter-fn (phrases/highlighter dictionary)
          anns (highlighter-fn txt)]
      (is (empty? anns)))
    (let [txt "before start end after"
          dictionary [{:text "end start" :id "1" :slop 2}]
          highlighter-fn (phrases/highlighter dictionary)
          anns (highlighter-fn txt)]
      (is (= 1 (count anns)))
      (is (= "start end" (:text (first anns))))))
  (testing "all terms should match despite the slop"
    (let [txt "before start end after"
          dictionary [{:text "end start foo" :id "1" :slop 100}]
          highlighter-fn (phrases/highlighter dictionary)
          anns (highlighter-fn txt)]
      (is (empty? anns)))))

(deftest dictionary-corner-cases
  (let [txt "Some text to test ."
        dictionary [{:text "."} {:text "text"}]
        highlighter-fn (phrases/highlighter dictionary {:tokenizer :whitespace})
        anns (highlighter-fn txt)]
    (is (= 2 (count anns))))
  (let [txt "Some text to test."
        dictionary [{:text "<html></html>"} {:text "text"}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn txt)]
    (is (seq anns))))

(deftest ^:noisy noisy-tests-for-corner-cases
  (let [txt "Some text to test."
        dictionary [{:text "."} {:text "text"}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn txt)]
    (is (seq anns))
    (is (= 1 (count anns))))
  (let [txt " `  `"
        dictionary [{:text "test" :id "1"}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn txt)]
    (is (coll? anns))
    (is (empty? anns)))
  (testing "slop versions"
    (stest/unstrument `phrases/highlighter)
    (testing "nil slop"
      (let [txt "before start end after"
            dictionary [{:text "end start foo" :id "1" :slop nil}]
            highlighter-fn (phrases/highlighter dictionary)
            anns (highlighter-fn txt)]
        (is (empty? anns))))
    (testing "very big slop"
      (let [txt "before start end after"
            dictionary [{:text "end start foo" :id "1" :slop 1000000000000}]
            highlighter-fn (phrases/highlighter dictionary)
            anns (highlighter-fn txt)]
        (is (empty? anns))))
    (testing "slop with negative value"
      (let [txt "before start end after"
            dictionary [{:text "end start foo" :id "1" :slop -1}]
            highlighter-fn (phrases/highlighter dictionary)
            anns (highlighter-fn txt)]
        (is (empty? anns))))
    (stest/instrument `phrases/highlighter)))

(deftest tokenizer-conf
  (let [txt "URGENT! Do this immediately!"
        dictionary [{:text "URGENT" :id "a" :tokenizer :whitespace}
                    {:text "URGENT" :id "b" :tokenizer :standard}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn txt)]
    (is (= 1 (count anns)))
    (is (= "b" (:dict-entry-id (first anns)))))
  (let [txt "[URGENT!] Do this immediately!"
        dictionary [{:text "[URGENT!]" :id "a" :tokenizer :whitespace}
                    {:text "[URGENT!]" :id "b" :tokenizer :standard}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn txt)]
    (is (= 2 (count anns)))
    (is (= "[URGENT!]" (:text (first (filter #(= "a" (:dict-entry-id %)) anns)))))
    (is (= "URGENT" (:text (first (filter #(= "b" (:dict-entry-id %)) anns)))))))

(deftest preserve-order
  (let [txt "Token Mill"
        dictionary [{:text "Mill Token" :slop 2 :in-order? false}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn txt)]
    (is (= 1 (count anns))))
  (let [txt "Token Mill"
        dictionary [{:text "Mill Token" :slop 2 :in-order? true}]
        highlighter-fn (phrases/highlighter dictionary)
        anns (highlighter-fn txt)]
    (is (= 0 (count anns)))))
