(ns beagle.validator-test
  (:require [clojure.test :refer :all]
            [beagle.validator :as validator]))

(deftest basic-cases
  (is (seq (validator/valid-dictionary? [{:text "test" :id "1" :meta {:test "test"} :type "CUSTOM"}])))
  (is (nil? (validator/valid-dictionary? [{:id "1" :meta {:test "test"} :type "CUSTOM"}]))))
