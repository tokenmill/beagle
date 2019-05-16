# clj-luwak

## Components

- Phrase annotator
- Dictionary file readers (csv, json, edn)
- Dictionary validator
- Dictionary optimizer
- Annotation merger

## Phrase annotator usage

```clojure
(let [dictionary [{:text "to be annotated" :id "1"}]
      annotator (phrases/annotator dictionary "LABEL")]
  (annotator "before annotated to be annotated after annotated"))
;=> ({:Text "to be annotated", :Type "LABEL", :DictEntryId "1", :Meta {}, :BeginOffset 17, :EndOffset 32})

(let [dictionary [{:text "TO BE ANNOTATED" :id "1" :case-sensitive? false}]
      annotator (phrases/annotator dictionary "LABEL")]
  (annotator "before annotated to be annotated after annotated"))
;=> ({:Text "to be annotated", :Type "LABEL", :DictEntryId "1", :Meta {}, :BeginOffset 17, :EndOffset 32})

(let [dictionary [{:text "TÖ BE ÄNNÖTÄTED" :id "1" :case-sensitive? false :ascii-fold? true}]
      annotator (phrases/annotator dictionary "LABEL")]
  (annotator "before annotated to be annotated after annotated"))
;=> ({:Text "to be annotated", :Type "LABEL", :DictEntryId "1", :Meta {}, :BeginOffset 17, :EndOffset 32})
```

## CSV dictionary format

Separator: ","
Escape: "\""

The first line *MUST* be a header.

Supported header keys: `["text" "type" "id" "synonyms" "case-sensitive?" ":ascii-fold?" "meta"]`

Order is not important.

Under `synonyms`, there should be a list of string separated by ";"
Under `meta`, there should be a list of strings separated by ";". Even number of strings is expected. In case of odd number, last one is ignored.

## Validator

Accepts any number of dictionaries to validate as long as they are provided in pairs as '"path/to/dicionary/file" "file-type"'

### Supported file types

- csv
- json
- edn

### Output

- If any dictionary is invalid exception will be thrown with exit status 1

### Usage

#### Clojure

To use validator directly execute command: `clj -m luwak.validator "/path/to/dictionary/file" "file-type" "/path/to/dictionary/file2" "file-type" & ...`

##### Example:

```
clj -m luwak.validator "your-dict.csv" "csv" "your-other-dict.json" "json"
```

#### Docker

Example in Gitlab CI:
```
validate-dictionary:
  stage: dictionary-validation
  when: manual
  image: registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator
  script:
    - /opt/validate your-dict.csv csv
    - /opt/validate your-dict.json json
    - /opt/validate your-dict.edn edn
```
