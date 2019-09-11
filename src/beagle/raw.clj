(ns beagle.raw
  (:require [clojure.string :as s]
            [beagle.monitor :as monitor]))

(defn annotate-text [text monitor field-names type-name])

(defn dictionary->monitor-queries [dictionary default-analysis-conf]
  [])

(defn annotator
  ([dictionary] (annotator dictionary {}))
  ([dictionary {:keys [type-name tokenizer]}]
   (let [type-name (if (s/blank? type-name) "PHRASE" type-name)
         {:keys [monitor field-names]} (monitor/setup dictionary
                                                      {:tokenizer tokenizer}
                                                      dictionary->monitor-queries)]
     (fn [text opts]
       (if (s/blank? text)
         []
         (annotate-text text monitor field-names type-name))))))
