(ns luwak.phrases-test
  (:require [clojure.test :refer :all]
            [luwak.phrases :as phrases]
            [schema.core :as s]
            [luwak.schema :as schema]))

(def label "LABEL")

(deftest type-per-dictionary-entry
  (let [dictionary [{:text "test phrase" :id "1" :meta {:test "test"} :type "CUSTOM"}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated test phrase after annotated")]
    (is (seq (s/validate schema/Annotations anns)))
    (is (not (empty? anns)))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "CUSTOM" (-> anns first :type)))
    (is (= "test phrase" (-> anns first :text)))
    (is (nil? (-> anns first (get-in [:meta "_type"]))))))

(deftest id
  (let [dictionary [{:text "test" :id "1" :meta {:test "test"}}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated test after annotated")]
    (is (not (empty? anns)))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "LABEL" (-> anns first :type)))))

(deftest metadata-append
  (let [dictionary [{:text "test" :meta {"email" "test@example.com"}}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated test after annotated")]
    (is (not (empty? anns)))
    (is (= {"email" "test@example.com"} (-> anns first :meta)))))

(deftest case-sensitivity
  (testing "case sensitive"
    (let [dictionary [{:text "test"}]
          annotator (phrases/annotator dictionary label)
          anns (annotator "before annotated test after annotated")]
      (is (not (empty? anns))))
    (let [dictionary [{:text "TeSt" :case-sensitive? true}]
          annotator (phrases/annotator dictionary label)
          anns (annotator "before annotated test after annotated")]
      (is (empty? anns)))
    (let [label "LABEL"
          dictionary [{:text "test" :case-sensitive? true}]
          annotator (phrases/annotator dictionary label)
          anns (annotator "before annotated Test after annotated")]
      (is (empty? anns))))

  (testing "case insensitive"
    (let [dictionary [{:text "TeSt" :case-sensitive? false}]
          annotator (phrases/annotator dictionary label)
          anns (annotator "before annotated test after annotated")]
      (is (not (empty? anns))))
    (let [dictionary [{:text "test" :case-sensitive? false}]
          annotator (phrases/annotator dictionary label)
          anns (annotator "before annotated test after annotated")]
      (is (not (empty? anns))))))

(deftest ascii-folding-dictionary
  (let [dictionary [{:text "wörd"}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated wörd after annotated")]
    (is (not (empty? anns))))
  (let [dictionary [{:text "wörd"}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated word after annotated")]
    (is (empty? anns)))
  (let [label "LABEL"
        dictionary [{:text "wörd" :ascii-fold? true}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated word after annotated")]
    (is (not (empty? anns))))
  (let [dictionary [{:text "word" :ascii-fold? true}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated wörd after annotated")]
    (is (not (empty? anns))))
  (let [label "LABEL"
        dictionary [{:text "word" :ascii-fold? false}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated wörd after annotated")]
    (is (empty? anns))))

(deftest ascii-folding-with-case-sensitivity
  (let [label "TYPE"]
    (testing "case sensitive"
      (let [dictionary [{:text "schön" :ascii-fold? true}]
            annotator (phrases/annotator dictionary label)
            anns (annotator "before annotated Schön after annotated")]
        (is (empty? anns)))
      (let [dictionary [{:text "Schön" :ascii-fold? true}]
            annotator (phrases/annotator dictionary label)
            anns (annotator "before annotated Schon after annotated")]
        (is (not (empty? anns))))
      (let [dictionary [{:text "schön" :ascii-fold? true}]
            annotator (phrases/annotator dictionary label)
            anns (annotator "before annotated Schon after annotated")]
        (is (empty? anns))))

    (testing "case insensitive"
      (let [dictionary [{:text "schön" :ascii-fold? true :case-sensitive? false}]
            annotator (phrases/annotator dictionary label)
            anns (annotator "before annotated Schon after annotated")]
        (is (not (empty? anns)))))
    (let [dictionary [{:text "schön" :ascii-fold? true :case-sensitive? false}]
          annotator (phrases/annotator dictionary label)
          anns (annotator "before annotated schon after annotated")]
      (is (not (empty? anns))))
    (let [dictionary [{:text "schon" :ascii-fold? true :case-sensitive? false}]
          annotator (phrases/annotator dictionary label)
          anns (annotator "before annotated schön after annotated")]
      (is (not (empty? anns))))

    (testing "false ascii fold"
      (let [dictionary [{:text "schon" :ascii-fold? false}]
            annotator (phrases/annotator dictionary label)
            anns (annotator "before annotated schön after annotated")]
        (is (empty? anns))))))

(deftest synonyms
  (let [dictionary [{:text "test" :id "1" :synonyms ["luwak"]}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated luwak after annotated")]
    (is (not (empty? anns)))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "luwak" (-> anns first :text))))

  (let [dictionary [{:text "test" :id "1" :synonyms ["Luwak"] :case-sensitive? true}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated luwak after annotated")]
    (is (empty? anns)))

  (let [dictionary [{:text "test" :id "1" :synonyms ["Luwak"] :case-sensitive? false}]
        annotator (phrases/annotator dictionary label)
        anns (annotator "before annotated luwak after annotated")]
    (is (not (empty? anns)))
    (is (= "1" (-> anns first :dict-entry-id)))
    (is (= "luwak" (-> anns first :text))))

  (testing "synonyms with false ascii fold"
    (let [dictionary [{:text "test" :synonyms ["schön"] :ascii-fold? false}]
          annotator (phrases/annotator dictionary label)
          anns (annotator "before annotated schon after annotated")]
      (is (empty? anns)))
    (let [dictionary [{:text "test" :synonyms ["schön"] :ascii-fold? true}]
          annotator (phrases/annotator dictionary label)
          anns (annotator "before annotated schon after annotated")]
      (is (not (empty? anns)))
      (is (= "schon" (-> anns first :text))))))
