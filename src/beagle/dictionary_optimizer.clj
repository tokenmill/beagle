(ns beagle.dictionary-optimizer
  (:require [clojure.set :as set]
            [clojure.tools.logging :as log]))

(def optimization-log (atom nil))

(defn merge-synonyms [group-of-entries]
  (reduce (fn [synonyms-set {synonyms :synonyms}]
            (into synonyms-set synonyms))
          #{} group-of-entries))

(defn merge-meta [group-of-entries]
  (reduce (fn [acc {meta :meta}] (merge acc meta)) {} group-of-entries))

(defn merge-entries [entries]
  (let [{:keys [text case-sensitive? ascii-fold? id entry-id]} (first entries)
        synonyms (remove #(= text %) (merge-synonyms entries))
        meta (merge-meta entries)]
    (cond-> {:text text :entry-id entry-id}
            (not-empty synonyms) (assoc :synonyms synonyms)
            (not-empty meta) (assoc :meta meta)
            id (assoc :id id)
            (not (nil? case-sensitive?)) (assoc :case-sensitive? case-sensitive?)
            (not (nil? ascii-fold?)) (assoc :ascii-fold? ascii-fold?))))

(defn mergeable-meta? [{meta-a :meta} {meta-b :meta}]
  (every? #(= (get meta-a %) (get meta-b %)) (set/intersection (set (keys meta-a)) (set (keys meta-b)))))

(defn possible-optimizations [{synonyms-a :synonyms text :text :as entry-a} {synonyms-b :synonyms :as entry-b}]
  (cond-> {}
          (= (dissoc entry-a :id :entry-id) (dissoc entry-b :id :entry-id)) (conj {:identical true})
          (some #(= text %) synonyms-a) (conj {:entry-a-text-equal-synonym true})
          (some #(= text %) synonyms-b) (conj {:entry-b-text-equal-synonym true})
          (set/superset? (set synonyms-a) (set synonyms-b)) (conj {:entry-a-superset true})
          (set/superset? (set synonyms-b) (set synonyms-a)) (conj {:entry-b-superset true})))

(defn log-optimization [{entry-a-id :entry-id :as entry-a} {entry-b-id :entry-id :as entry-b}]
  (let [{:keys [identical entry-a-text-equal-synonym entry-b-text-equal-synonym entry-a-superset entry-b-superset] :as optimizations}
        (possible-optimizations entry-a entry-b)]
    (swap! optimization-log conj
           (cond-> ""
                   identical (str (format "dictionary item '%s' and '%s' are identical" entry-a-id entry-b-id))
                   entry-a-text-equal-synonym (str (format "dictionary item '%s' has synonym equal to its text" entry-a-id))
                   entry-b-text-equal-synonym (str (format "dictionary item '%s' has synonym equal to its text" entry-b-id))
                   entry-a-superset (str (format "dictionary item '%s' synonyms are superset of item '%s' synonyms list - mergeable" entry-a-id entry-b-id))
                   entry-b-superset (str (format "dictionary item '%s' synonyms are superset of item '%s' synonyms list - mergeable" entry-b-id entry-a-id))
                   (empty? optimizations) (str (format "dictionary item '%s' and '%s' differ only by synonyms list - mergeable" entry-a-id entry-b-id))))))

(defn aggregate-entries-by-meta [entries]
  (loop [entry-a (first entries)
         [entry-b & remaining] (rest entries)
         acc []
         exceptions []]
    (if entry-b
      (if (mergeable-meta? entry-a entry-b)
        (do (log-optimization entry-a entry-b)
            (recur (merge-entries [entry-a entry-b]) remaining acc exceptions))
        (recur entry-a remaining acc (conj exceptions entry-b)))
      (if (seq exceptions)
        (recur (first exceptions) (rest exceptions) (conj acc entry-a) [])
        (conj acc entry-a)))))

(defn add-id-to-entries [dictionary]
  (loop [entry (first dictionary)
         remaining (rest dictionary)
         result []
         entry-id 0]
    (if entry
      (recur (first remaining) (rest remaining) (conj result (assoc entry :entry-id entry-id)) (inc entry-id))
      result)))

(defn optimize-dictionary [dictionary]
  (mapcat (fn [[_ grouped-entries]]
            (aggregate-entries-by-meta grouped-entries))
          (group-by (fn [entry] [(:text entry) (:case-sensitive? entry) (:ascii-fold? entry)]) (add-id-to-entries dictionary))))

(defn optimize
  ([dictionary]
   (reset! optimization-log [])
   (println "Optimized entries ->> " @optimization-log)
   (optimize-dictionary dictionary))
  ([dictionary dry-run?]
   (reset! optimization-log [])
   (let [optimized (optimize-dictionary dictionary)]
     (if dry-run?
       (do (log/infof "Possible optimizations:\n '%s'" @optimization-log)
           dictionary)
       (do (log/infof "Optimized dictionary items:\n '%s'" @optimization-log)
           optimized)))))
