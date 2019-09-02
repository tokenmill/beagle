(ns beagle.text-analysis-test
  (:require [clojure.test :refer [deftest is]]
            [beagle.text-analysis :as text-analysis]))

(deftest conf-analysis
  (is (= #{} (text-analysis/conf->analyzers {})))
  (is (= #{:lowercase} (text-analysis/conf->analyzers {:case-sensitive? false})))
  (is (= #{} (text-analysis/conf->analyzers {:case-sensitive? true})))
  (is (= #{:ascii-fold} (text-analysis/conf->analyzers {:ascii-fold? true})))
  (is (= #{} (text-analysis/conf->analyzers {:ascii-fold? false})))
  (is (= #{:stem} (text-analysis/conf->analyzers {:stem? true})))
  (is (= #{} (text-analysis/conf->analyzers {:stem? false})))
  (is (= #{:ascii-fold :lowercase}
         (text-analysis/conf->analyzers {:case-sensitive? false
                                         :ascii-fold? true})))
  (is (= #{:ascii-fold}
         (text-analysis/conf->analyzers {:case-sensitive? true
                                         :ascii-fold? true})))
  (is (= #{:stem :ascii-fold}
         (text-analysis/conf->analyzers {:stem? true
                                         :ascii-fold? true})))
  (is (= #{:stem :ascii-fold :lowercase}
         (text-analysis/conf->analyzers {:stem? true
                                         :case-sensitive? false
                                         :ascii-fold? true}))))
