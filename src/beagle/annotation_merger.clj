(ns beagle.annotation-merger)

(defn related-annotations? [anno1 anno2]
  (<= (:begin-offset anno1) (:begin-offset anno2) (:end-offset anno1)))

(defn parent-child-annotations? [parent-anno child-anno]
  (and (>= (:begin-offset child-anno) (:begin-offset parent-anno))
       (<= (:end-offset child-anno) (:end-offset parent-anno))))

(defn merge-annotations [annotations]
  (let [sorted-annotation (sort-by :begin-offset annotations)]
    (loop [parent-annotation (first sorted-annotation)
           [child-annotation & remaining] (rest sorted-annotation)
           result []]
      (if child-annotation
        (if (related-annotations? parent-annotation child-annotation)
          (recur (if (and (parent-child-annotations? parent-annotation child-annotation)
                          (not (parent-child-annotations? child-annotation parent-annotation)))
                   parent-annotation
                   child-annotation)
                 remaining
                 result)
          (recur child-annotation remaining (conj result parent-annotation)))
        (conj result parent-annotation)))))

(defn merge-same-type-annotations [annotations]
  (mapcat (fn [[_ anns]] (merge-annotations anns)) (group-by :type annotations)))
