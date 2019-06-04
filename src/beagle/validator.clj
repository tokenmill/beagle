(ns beagle.validator
  (:gen-class)
  (:require [schema.core :as s]
            [beagle.schema :as sch]
            [beagle.readers :as readers]))

(defn valid-dictionary? [dictionary]
  (try
    (seq (s/validate sch/Dictionary dictionary))
    (catch Exception _)))

(def supported-dictionary-file-types #{"csv" "json" "edn"})

(defn valid-dictionary-file? [dictionary-file dictionary-file-type]
  (if (contains? supported-dictionary-file-types dictionary-file-type)
    (valid-dictionary? (case dictionary-file-type
                         "csv" (readers/read-csv dictionary-file)
                         "json" (readers/read-json dictionary-file)
                         "edn" (readers/read-edn dictionary-file)))
    (.printStackTrace (Exception. (format "File type not supported: `%s`" dictionary-file-type)))))

(defn -main [& args]
  (when (odd? (count args))
    (.printStackTrace (Exception. "Even number of arguments must be present - 'dictionary-name dictionary-type ...'"))
    (System/exit 1))
  (when (some #(not (apply valid-dictionary-file? %)) (partition-all 2 args))
    (System/exit 1)))
