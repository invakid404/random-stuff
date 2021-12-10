(require '[clojure.string :as str]
         '[kixi.stats.core :refer (median)])

(def pairs {\) \(
            \] \[
            \} \{
            \> \<})

(def invalid-map {\) 3
                  \] 57
                  \} 1197
                  \> 25137})

(def incomplete-map {\( 1
                     \[ 2
                     \{ 3
                     \< 4})

(defn incomplete-score [stack]
  (->> stack
       reverse
       (reduce (fn [acc curr-char]
                 (+ (* acc 5) (incomplete-map curr-char)))
               0)))

(defn score
  ([string] (score string []))
  ([[first-char & other :as string] stack]
    (cond
      (empty? string) {:tag :incomplete :value (incomplete-score stack)}
      (pairs first-char) (if (= (pairs first-char) (peek stack))
                           (recur other (pop stack))
                           {:tag :invalid :value (invalid-map first-char)})
      :else (recur other (conj stack first-char)))))

(defn solve [input target-key reduce-fn]
  (->> (get input target-key)
       (transduce (map :value) reduce-fn)
       int))

(def input
  (->> (slurp "input.txt")
       str/split-lines
       (map score)
       (group-by #(get % :tag))))

(println (solve input :invalid +) (solve input :incomplete median))
