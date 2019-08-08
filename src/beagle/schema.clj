(ns beagle.schema
  (:require [clojure.spec.alpha :as s]))

(s/def ::text string?)
(s/def ::type string?)
(s/def ::id string?)
(s/def ::synonyms (s/coll-of string?))
(s/def ::case-sensitive? boolean?)
(s/def ::ascii-fold? boolean?)
(s/def ::meta (s/map-of #(or (string? %) (keyword? %)) string?))

(s/def ::dict-entry
  (s/keys :req-un [::text]
          :opt-un [::type ::id ::synonyms ::meta
                   ::case-sensitive? ::ascii-fold?]))

(s/def ::dictionary (s/coll-of ::dict-entry))

(s/def ::begin-offset pos-int?)
(s/def ::end-offset pos-int?)
(s/def ::dict-entry-id string?)

(s/def ::dictionary-annotation
  (s/keys :req-un [::text ::type ::begin-offset ::end-offset]
          :opt-un [::dict-entry-id ::meta]))

(s/def ::annotations (s/coll-of ::dictionary-annotation))
