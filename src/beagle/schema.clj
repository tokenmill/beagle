(ns beagle.schema
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]))

(s/def ::non-empty-string
  (s/and string? (complement str/blank?)))

(s/def ::text ::non-empty-string)
(s/def ::type string?)
(s/def ::id string?)
(s/def ::synonyms (s/coll-of ::non-empty-string))
(s/def ::case-sensitive? boolean?)
(s/def ::ascii-fold? boolean?)
(s/def ::meta
  (s/with-gen
    (s/map-of #(or (string? %) (keyword? %)) string?)
    #(gen/fmap (fn [s] {s s}) (s/gen string?))))

(s/def ::dict-entry
  (s/keys :req-un [::text]
          :opt-un [::type ::id ::synonyms ::meta
                   ::case-sensitive? ::ascii-fold?]))

(s/def ::dictionary (s/coll-of ::dict-entry))

(s/def ::begin-offset pos-int?)
(s/def ::end-offset pos-int?)
(s/def ::dict-entry-id ::non-empty-string)

(s/def ::dictionary-annotation
  (s/keys :req-un [::text ::type ::begin-offset ::end-offset]
          :opt-un [::dict-entry-id ::meta]))

(s/def ::annotations (s/coll-of ::dictionary-annotation))
