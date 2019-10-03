(ns beagle.schema
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]))

(s/def ::non-empty-string
  (s/and string? (complement str/blank?)))

(s/def ::text ::non-empty-string)
(s/def ::type (s/nilable string?))
(s/def ::id (s/nilable string?))
(s/def ::synonyms (s/nilable (s/coll-of ::non-empty-string)))
(s/def ::case-sensitive? (s/nilable boolean?))
(s/def ::ascii-fold? (s/nilable boolean?))
(s/def ::stem? (s/nilable boolean?))
(s/def ::stemmer (s/nilable keyword?))
(s/def ::slop (s/nilable #(and (number? %) (or (pos-int? %) (zero? %)))))
(s/def ::meta
  (s/with-gen
    (s/nilable (s/map-of #(or (string? %) (keyword? %)) string?))
    #(gen/fmap (fn [s] {s s}) (s/gen string?))))

(s/def ::dict-entry
  (s/keys :req-un [::text]
          :opt-un [::type ::id ::synonyms ::meta
                   ::case-sensitive? ::ascii-fold? ::stem? ::stemmer ::slop]))

(defrecord DictionaryEntry [text type id synonyms case-sensitive? ascii-fold?
                            stem? stemmer slop meta])

(s/def ::dictionary (s/coll-of ::dict-entry))

(s/def ::begin-offset pos-int?)
(s/def ::end-offset pos-int?)
(s/def ::dict-entry-id ::non-empty-string)

(s/def ::dictionary-annotation
  (s/keys :req-un [::text ::type ::begin-offset ::end-offset]
          :opt-un [::dict-entry-id ::meta]))

(defrecord Highlight [text type dict-entry-id meta begin-offset end-offset])

(s/def ::annotations (s/coll-of ::dictionary-annotation))
