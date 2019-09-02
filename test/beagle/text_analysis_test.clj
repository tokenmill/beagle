(ns beagle.text-analysis-test
  (:require [clojure.test :refer [deftest is]]
            [beagle.text-analysis :as text-analysis]))

(deftest conf-analysis
  (is (= #{:stem} (text-analysis/conf->analyzers {:stem? true}))))
