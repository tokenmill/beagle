(ns luwak.schema
  (:require [schema.core :as s]))

(s/defschema DictEntry
  {:text                             s/Str
   (s/optional-key :type)            s/Str
   (s/optional-key :id)              s/Str
   (s/optional-key :synonyms)        [s/Str]
   (s/optional-key :case-sensitive?) s/Bool
   (s/optional-key :ascii-fold?)     s/Bool
   (s/optional-key :meta)            {(s/cond-pre s/Keyword s/Str) s/Str}})

(s/defschema Dictionary [DictEntry])

(s/defschema DictionaryAnnotation
  {:type                           s/Str
   :text                           s/Str
   :begin-offset                   s/Num
   :end-offset                     s/Num
   (s/optional-key :dict-entry-id) s/Str
   (s/optional-key :meta)          {(s/cond-pre s/Keyword s/Str) s/Str}})

(s/defschema Annotations [DictionaryAnnotation])
