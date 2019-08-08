(ns beagle.readers
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [cheshire.core :as json])
  (:import (java.io PushbackReader)))

(defn read-edn
  "Reads dictionary from the source.
  `source` - must be something that an input stream can be created."
  [source]
  (with-open [rdr (PushbackReader. (io/reader (io/input-stream source)))]
    (doall (edn/read rdr))))

(defn read-csv [source]
  (with-open [reader (io/reader source)]
    (let [[header & lines] (csv/read-csv reader :separator \, :quote \")
          kvs (map keyword header)]
      (->> lines
           (map #(apply hash-map (interleave kvs %)))
           (map #(into {} (remove (fn [[_ v]] (s/blank? v)) %)))
           (map (fn [{:keys [synonyms] :as dict}]
                  (if-not (s/blank? synonyms)
                    (assoc dict :synonyms (map s/trim (s/split synonyms #";")))
                    dict)))
           (map (fn [{:keys [case-sensitive?] :as dict}]
                  (if-not (s/blank? case-sensitive?)
                    (assoc dict :case-sensitive? (Boolean/valueOf ^String case-sensitive?))
                    dict)))
           (map (fn [{:keys [ascii-fold?] :as dict}]
                  (if-not (s/blank? ascii-fold?)
                    (assoc dict :ascii-fold? (Boolean/valueOf ^String ascii-fold?))
                    dict)))
           (map (fn [{:keys [meta] :as dict}]
                  (if-not (s/blank? meta)
                    (assoc dict :meta (reduce (fn [acc [k v]] (assoc acc k v))
                                              {}
                                              (->> (map s/trim (s/split meta #";"))
                                                   (partition-all 2)
                                                   (remove (fn [[_ v]] (s/blank? (str v)))))))

                    dict)))
           (doall)))))

(defn read-json [source]
  (with-open [rdr (io/reader (io/input-stream source))]
    (doall (json/decode-stream rdr true))))
