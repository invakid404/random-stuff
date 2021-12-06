(require '[clojure.string :as str])

(defn add? [a b]
  (+ (or a 0) (or b 0)))

(defn next-state [data]
  (reduce-kv (fn [acc curr-value curr-count]
               (if (= curr-value 0)
                 (-> acc (update 6 add? curr-count) (update 8 add? curr-count))
                 (update acc (dec curr-value) add? curr-count)))
             {} data))

(defn solve [data days]
  (->> (iterate next-state data)
       (#(nth % days))
       vals
       (reduce +)))

(def input
  (as-> (slurp "input.txt") data
        (str/trim data)
        (str/split data #",")
        (map #(Integer. %) data)
        (frequencies data)))

(println (solve input 80) (solve input 256))
