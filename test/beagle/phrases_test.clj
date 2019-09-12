(ns beagle.phrases-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [beagle.phrases :as phrases]
            [beagle.schema :as schema]))

(s/def ::opts (s/* (s/cat :opt keyword? :val any?)))

(s/fdef phrases/annotator
        :args (s/cat :dictionary ::schema/dictionary
                     :kwargs (s/* (s/cat :opt keyword? :val any?)))
        :ret (s/fspec :args (s/cat :text string?
                                   :kwargs (s/* (s/cat :opt keyword? :val any?)))
                      :ret ::schema/annotations))

(stest/instrument`phrases/annotator)

(s/exercise-fn `phrases/annotator)

(def label "LABEL")

(deftest type-per-dictionary-entry
  (let [dictionary [{:text "test phrase" :id "1" :meta {:test "test"} :type "CUSTOM"}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated test phrase after annotated")]
    (is (seq (s/conform ::schema/annotations anns)))
    (is (seq anns))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "CUSTOM" (-> anns first :type)))
    (is (= "test phrase" (-> anns first :text)))
    (is (nil? (-> anns first (get-in [:meta "_type"]))))))

(deftest id
  (let [dictionary [{:text "test" :id "1" :meta {:test "test"}}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated test after annotated")]
    (is (seq anns))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "LABEL" (-> anns first :type)))))

(deftest metadata-append
  (let [dictionary [{:text "test" :meta {"email" "test@example.com"}}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated test after annotated")]
    (is (seq anns))
    (is (= {"email" "test@example.com"} (-> anns first :meta)))))

(deftest case-sensitivity
  (testing "case sensitive"
    (let [dictionary [{:text "test"}]
          annotator (phrases/annotator dictionary :type-name label)
          anns (annotator "before annotated test after annotated")]
      (is (seq anns)))
    (let [dictionary [{:text "TeSt" :case-sensitive? true}]
          annotator (phrases/annotator dictionary :type-name label)
          anns (annotator "before annotated test after annotated")]
      (is (empty? anns)))
    (let [label "LABEL"
          dictionary [{:text "test" :case-sensitive? true}]
          annotator (phrases/annotator dictionary :type-name label)
          anns (annotator "before annotated Test after annotated")]
      (is (empty? anns))))

  (testing "case insensitive"
    (let [dictionary [{:text "TeSt" :case-sensitive? false}]
          annotator (phrases/annotator dictionary :type-name label)
          anns (annotator "before annotated test after annotated")]
      (is (seq anns)))
    (let [dictionary [{:text "test" :case-sensitive? false}]
          annotator (phrases/annotator dictionary :type-name label)
          anns (annotator "before annotated test after annotated")]
      (is (seq anns)))))

(deftest ascii-folding-dictionary
  (let [dictionary [{:text "wörd"}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated wörd after annotated")]
    (is (seq anns)))
  (let [dictionary [{:text "wörd"}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated word after annotated")]
    (is (empty? anns)))
  (let [label "LABEL"
        dictionary [{:text "wörd" :ascii-fold? true}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated word after annotated")]
    (is (seq anns)))
  (let [dictionary [{:text "word" :ascii-fold? true}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated wörd after annotated")]
    (is (seq anns)))
  (let [label "LABEL"
        dictionary [{:text "word" :ascii-fold? false}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated wörd after annotated")]
    (is (empty? anns))))

(deftest ascii-folding-with-case-sensitivity
  (let [label "TYPE"]
    (testing "case sensitive"
      (let [dictionary [{:text "schön" :ascii-fold? true}]
            annotator (phrases/annotator dictionary :type-name label)
            anns (annotator "before annotated Schön after annotated")]
        (is (empty? anns)))
      (let [dictionary [{:text "Schön" :ascii-fold? true}]
            annotator (phrases/annotator dictionary :type-name label)
            anns (annotator "before annotated Schon after annotated")]
        (is (seq anns)))
      (let [dictionary [{:text "schön" :ascii-fold? true}]
            annotator (phrases/annotator dictionary :type-name label)
            anns (annotator "before annotated Schon after annotated")]
        (is (empty? anns))))

    (testing "case insensitive"
      (let [dictionary [{:text "schön" :ascii-fold? true :case-sensitive? false}]
            annotator (phrases/annotator dictionary :type-name label)
            anns (annotator "before annotated Schon after annotated")]
        (is (seq anns))))
    (let [dictionary [{:text "schön" :ascii-fold? true :case-sensitive? false}]
          annotator (phrases/annotator dictionary :type-name label)
          anns (annotator "before annotated schon after annotated")]
      (is (seq anns)))
    (let [dictionary [{:text "schon" :ascii-fold? true :case-sensitive? false}]
          annotator (phrases/annotator dictionary :type-name label)
          anns (annotator "before annotated schön after annotated")]
      (is (seq anns)))

    (testing "false ascii fold"
      (let [dictionary [{:text "schon" :ascii-fold? false}]
            annotator (phrases/annotator dictionary :type-name label)
            anns (annotator "before annotated schön after annotated")]
        (is (empty? anns))))))

(deftest synonyms
  (let [dictionary [{:text "test" :id "1" :synonyms ["beagle"]}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated beagle after annotated")]
    (is (seq anns))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "beagle" (-> anns first :text))))

  (let [dictionary [{:text "test" :id "1" :synonyms ["Luwak"] :case-sensitive? true}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated beagle after annotated")]
    (is (empty? anns)))

  (let [dictionary [{:text "test" :id "1" :synonyms ["beagle"] :case-sensitive? false}]
        annotator (phrases/annotator dictionary :type-name label)
        anns (annotator "before annotated beagle after annotated")]
    (is (seq anns))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "beagle" (-> anns first :text))))

  (testing "synonyms with false ascii fold"
    (let [dictionary [{:text "test" :synonyms ["schön"] :ascii-fold? false}]
          annotator (phrases/annotator dictionary :type-name label)
          anns (annotator "before annotated schon after annotated")]
      (is (empty? anns)))
    (let [dictionary [{:text "test" :synonyms ["schön"] :ascii-fold? true}]
          annotator (phrases/annotator dictionary :type-name label)
          anns (annotator "before annotated schon after annotated")]
      (is (seq anns))
      (is (= "schon" (-> anns first :text))))))

(deftest phrase-end-sentence
  (let [dictionary [{:text "test-test"}]
        annotator (phrases/annotator dictionary)
        anns (annotator "before annotated test-test.")]
    (is (seq anns))
    (is (= "test-test" (:text (first anns))))))

(deftest phrase-in-quotes
  (let [dictionary [{:text "test-test" :case-sensitive? false}]
        annotator (phrases/annotator dictionary)
        anns (annotator "before annotated \"TEST-test\".")]
    (is (seq anns))
    (is (= "TEST-test" (:text (first anns))))))

(deftest phrase-in-quotes-should-not-match
  (let [dictionary [{:text "test-test" :case-sensitive? false}]
        annotator (phrases/annotator dictionary :tokenizer :whitespace)
        anns (annotator "before annotated \"TEST-test\".")]
    (is (empty? anns))))

(deftest overlapping-phrases
  (let [dictionary [{:text "test phrase test" :case-sensitive? false}]
        annotator (phrases/annotator dictionary :tokenizer :whitespace)
        anns (annotator "start test phrase test phrase test end")]
    (is (= 2 (count anns)))))

(deftest lt-stemming
  (let [dictionary [{:text "Kaunas" :id "1" :stem? true :stemmer :lithuanian}]
        annotator-fn (phrases/annotator dictionary)
        anns (annotator-fn "Kauno miestas")]
    (is (seq anns))
    (is (= "Kauno" (-> anns first :text))))
  (let [dictionary [{:text "Kaunas Vilnius" :id "1" :stem? true}]
        annotator-fn (phrases/annotator dictionary)
        anns (annotator-fn "Kaunas, Vilnius")]
    (is (seq anns))
    (is (= "Kaunas, Vilnius" (-> anns first :text))))
  (let [dictionary [{:text "Kaunas" :id "1" :case-sensitive? false :stem? true :stemmer :lithuanian}]
        annotator-fn (phrases/annotator dictionary)
        anns (annotator-fn "kauno miestas")]
    (is (seq anns))
    (is (= "kauno" (-> anns first :text)))))

(deftest en-stemming
  (let [txt "who let the dogs out?"]
    (let [dictionary [{:text "dog" :id "1"}]
          annotator-fn (phrases/annotator dictionary)
          anns (annotator-fn txt)]
      (is (empty? anns)))
    (let [dictionary [{:text "dog" :id "1" :stem? true}]
          annotator-fn (phrases/annotator dictionary)
          anns (annotator-fn txt)]
      (is (seq anns))
      (is (= "dogs" (-> anns first :text))))
    (let [dictionary [{:text "dog" :id "1" :stem? true :stemmer :english}]
          annotator-fn (phrases/annotator dictionary)
          anns (annotator-fn txt)]
      (is (seq anns))
      (is (= "dogs" (-> anns first :text))))
    (let [dictionary [{:text "dog" :id "1" :stem? true :stemmer :estonian}]
          annotator-fn (phrases/annotator dictionary)
          anns (annotator-fn txt)]
      (is (empty? anns)))))

(deftest mixed-stemmers
  (let [txt "Saboniai plays basketball"
        dictionary [{:text "Sabonis" :id "1" :stem? true :stemmer :lithuanian}
                    {:text "play" :id "2" :stem? true :stemmer :english}]
        annotator-fn (phrases/annotator dictionary)
        anns (annotator-fn txt)]
    (is (= 2 (count anns)))))

(deftest phrase-slop
  (let [txt "before start and end after"
        dictionary [{:text "start end" :id "1" :slop 1}]
        annotator-fn (phrases/annotator dictionary)
        anns (annotator-fn txt)]
    (is (= 1 (count anns)))
    (is (= "start and end" (:text (first anns)))))
  (testing "all terms in the phrase should match"
    (let [txt "before start end after"
          dictionary [{:text "start NOPE end" :id "1" :slop 10}]
          annotator-fn (phrases/annotator dictionary)
          anns (annotator-fn txt)]
      (is (empty? anns))))
  (let [txt "before start phrase and end phrase after"
        dictionary [{:text "start phrase end phrase" :id "1" :slop 1}]
        annotator-fn (phrases/annotator dictionary)
        anns (annotator-fn txt)]
    (is (= 1 (count anns)))
    (is (= "start phrase and end phrase" (:text (first anns)))))
  (testing "phrase edit distance"
    (let [txt "before start end after"
          dictionary [{:text "end start" :id "1" :slop 0}]
          annotator-fn (phrases/annotator dictionary)
          anns (annotator-fn txt)]
      (is (empty? anns)))
    (let [txt "before start end after"
          dictionary [{:text "end start" :id "1" :slop 2}]
          annotator-fn (phrases/annotator dictionary)
          anns (annotator-fn txt)]
      (is (= 1 (count anns)))
      (is (= "start end" (:text (first anns))))))
  (testing "all terms should match despite the slop"
    (let [txt "before start end after"
          dictionary [{:text "end start foo" :id "1" :slop 100}]
          annotator-fn (phrases/annotator dictionary)
          anns (annotator-fn txt)]
      (is (empty? anns)))))

(deftest dictionary-corner-cases
  (let [txt "Some text to test ."
        dictionary [{:text "."} {:text "text"}]
        annotator-fn (phrases/annotator dictionary :tokenizer :whitespace)
        anns (annotator-fn txt)]
    (is (= (count anns))))
  (let [txt "Some text to test."
        dictionary [{:text "."} {:text "text"}]
        annotator-fn (phrases/annotator dictionary)
        anns (annotator-fn txt)]
    (is (seq anns))
    (is (= 1 (count anns))))
  (let [txt "Some text to test."
        dictionary [{:text "<html></html>"} {:text "text"}]
        annotator-fn (phrases/annotator dictionary)
        anns (annotator-fn txt)]
    (is (seq anns)))
  (let [txt "Some text to test."
        dictionary [{:text "` ` `"} {:text "text"}]
        annotator-fn (phrases/annotator dictionary)
        anns (annotator-fn txt)]
    (is (seq anns))))

(deftest handle-with-bad-text
  (let [txt " `  `"
        dictionary [{:text "test" :id "1"}]
        annotator-fn (phrases/annotator dictionary)
        anns (annotator-fn txt)]
    (is (coll? anns))
    (is (empty? anns))))
