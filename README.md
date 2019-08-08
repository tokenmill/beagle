# beagle

Detector of interesting things in text. Intended use is in the stream search applications. 

Implementations is based on [Lucene monitor](https://github.com/apache/lucene-solr/tree/master/lucene/monitor) which is based on [Luwak](https://github.com/flaxsearch/luwak).

## Components

- Phrase annotator
- Dictionary file readers (csv, json, edn)
- Dictionary validator
- Dictionary optimizer
- Annotation merger

## Phrase annotator usage

```clojure
(require '[beagle.phrases :as phrases])

(let [dictionary [{:text "to be annotated" :id "1"}]
      annotator (phrases/annotator dictionary :type-name "LABEL")]
  (annotator "before annotated to be annotated after annotated"))
=> ({:text "to be annotated", :type "LABEL", :dict-entry-id "1", :meta {}, :begin-offset 17, :end-offset 32})

(let [dictionary [{:text "TO BE ANNOTATED" :id "1" :case-sensitive? false}]
      annotator (phrases/annotator dictionary :type-name "LABEL")]
  (annotator "before annotated to be annotated after annotated"))
=> ({:text "to be annotated", :type "LABEL", :dict-entry-id "1", :meta {}, :begin-offset 17, :end-offset 32})

(let [dictionary [{:text "TÖ BE ÄNNÖTÄTED" :id "1" :case-sensitive? false :ascii-fold? true}]
      annotator (phrases/annotator dictionary :type-name "LABEL")]
  (annotator "before annotated to be annotated after annotated"))
=> ({:text "to be annotated", :type "LABEL", :dict-entry-id "1", :meta {}, :begin-offset 17, :end-offset 32})
```

## Dictionary readers

Three file formats are supported: csv, edn, json.

### CSV dictionary format

Separator: ","
Escape: "\""

The first line *MUST* be a header.

Supported header keys: `["text" "type" "id" "synonyms" "case-sensitive?" ":ascii-fold?" "meta"]`

Order is not important.

Under `synonyms`, there should be a list of string separated by ";"
Under `meta`, there should be a list of strings separated by ";". Even number of strings is expected. In case of odd number, last one is ignored.

## Validator

Accepts any number of dictionaries to validate as long as they are provided in pairs as '"/path/to/dictionary/file" "file-type"'

### Supported file types

- csv
- json
- edn

### Output

- If any dictionary is invalid exception will be thrown with exit status 1

### Usage

#### Clojure

To use validator directly execute command: `clj -m beagle.validator "/path/to/dictionary/file" "file-type" "/path/to/dictionary/file2" "file-type" & ...`

##### Example:

```
clj -m beagle.validator "your-dict.csv" "csv" "your-other-dict.json" "json"
```

#### Docker

Example in Gitlab CI:

```
validate-dictionaries:
  stage: dictionary-validation
  when: always
  image: registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator:2
  script:
    - >
      dictionary-validator
      /path/to/dict.csv csv
      /path/to/dict.json json
      /path/to/dict.edn edn
```

## Dictionary optimizer

Supported optimizations:
- Remove duplicate dictionary entries
- Merge synonyms
- Synonyms and text equality check

There are cases when dictionary entries can't be merged:
- Differences in text analysis

Examples:
```clojure
(require '[beagle.dictionary-optimizer :as optimizer])

; Remove duplicates
(let [dictionary [{:text "TO BE ANNOTATED" :id "1"}
                  {:text "TO BE ANNOTATED"}]]
  (optimizer/optimize dictionary))
=> ({:text "TO BE ANNOTATED", :id "1"})

; Merge synonyms
(let [dictionary [{:text "TO BE ANNOTATED" :synonyms ["ONE"]}
                  {:text "TO BE ANNOTATED" :synonyms ["TWO"]}]]
  (optimizer/optimize dictionary))
=> ({:text "TO BE ANNOTATED", :synonyms ("TWO" "ONE")})

; Synonyms and text equality check
(let [dictionary [{:text "TO BE ANNOTATED" :synonyms ["TO BE ANNOTATED"]}]]
  (optimizer/optimize dictionary))
=> ({:text "TO BE ANNOTATED", :synonyms ["TO BE ANNOTATED"]})

; Can't be merged because of differences in text analysis
(let [dictionary [{:text "TO BE ANNOTATED" :case-sensitive? true}
                  {:text "TO BE ANNOTATED" :case-sensitive? false}]]
  (optimizer/optimize dictionary))
=> ({:text "TO BE ANNOTATED", :case-sensitive? true} {:text "TO BE ANNOTATED", :case-sensitive? false})
```

## Annotation merger

Only annotations of the same type are merged.

Handled cases:
- Duplicate annotations
- Nested annotations

Examples:
```clojure
(require '[beagle.annotation-merger :as merger])

(let [dictionary [{:text "TEST"}
                  {:text "This TEST is"}]
      annotator (phrases/annotator dictionary)
      annotations (annotator "This TEST is")]
  (println "Annotations: " annotations)
  (merger/merge-same-type-annotations annotations))
Annotations:  ({:text TEST, :type PHRASE, :dict-entry-id 0, :meta {}, :begin-offset 5, :end-offset 9} {:text This TEST is, :type PHRASE, :dict-entry-id 1, :meta {}, :begin-offset 0, :end-offset 12})
=> ({:text "This TEST is", :type "PHRASE", :dict-entry-id "1", :meta {}, :begin-offset 0, :end-offset 12})
```
