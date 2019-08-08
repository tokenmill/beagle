(ns beagle.readers-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [beagle.schema :as sch]
            [beagle.readers :as readers])
  (:import (java.io ByteArrayInputStream)))

(deftest json-reader
  (is (not (nil? (s/conform ::sch/dictionary
                             (readers/read-json
                               (ByteArrayInputStream.
                                 (.getBytes "[{\"text\": \"moo\"}]")))))))
  (is (not (nil? (s/conform ::sch/dictionary
                             (readers/read-json "test/resources/dict.json"))))))

(deftest csv-file-reader
  (is (not (nil? (s/conform ::sch/dictionary (readers/read-csv "test/resources/dict.csv"))))))

(deftest edn-file-reader
  (is (not (nil? (s/conform ::sch/dictionary (readers/read-edn "test/resources/dict.edn"))))))
