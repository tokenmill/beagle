(ns beagle.java.annotation)

(gen-class
  :name lt.tokenmill.beagle.phrases.Annotation
  :prefix Annotation-
  :state "state"
  :init "init"
  :constructors {[String String Long Long String java.util.Map] []}
  :methods [[text [] String]
            [type [] String]
            [beginOffset [] Long]
            [endOffset [] Long]
            [dictionaryEntryId [] String]
            [meta [] java.util.Map]]
  :prefix Annotation-)

(defn Annotation-init [text type begin end dictionaryEntryId meta]
  [[] (atom {:text          text
             :type          type
             :begin         begin
             :end           end
             :dict-entry-id dictionaryEntryId
             :meta          meta})])

(defn Annotation-text [this]
  (@(.state this) :text))
(defn Annotation-type [this]
  (@(.state this) :type))
(defn Annotation-beginOffset [this]
  (@(.state this) :begin))
(defn Annotation-endOffset [this]
  (@(.state this) :end))
(defn Annotation-dictionaryEntryId [this]
  (@(.state this) :dict-entry-id))
(defn Annotation-meta [this]
  (@(.state this) :meta))
