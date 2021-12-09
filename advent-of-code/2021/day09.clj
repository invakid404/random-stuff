(require '[clojure.string :as str]
         '[loom.graph :refer (nodes digraph out-degree)]
         '[loom.attr :refer (add-attr attr)]
         '[loom.alg :refer (connected-components)])

(defn part1 [graph]
  (->> (nodes graph)
       (filter #(= (out-degree graph %) 0))
       (map #(attr graph % :value))
       (map inc)
       (reduce +)))

(defn part2 [graph]
  (->> (connected-components graph)
       (map count)
       sort
       reverse
       (take 3)
       (reduce *)))

(def deltas [[-1, 0] [1, 0] [0, -1] [0, 1]])

(defn index-1d [{:keys [cols]} [x, y]]
  (+ (* x cols) y))

(defn index-2d [{:keys [cols]} idx]
  (vector (quot idx cols) (rem idx cols)))

(defn neighbors [{:keys [rows cols] :as input} idx]
  (let [curr-index (index-2d input idx)]
    (->> deltas
         (map #(map + curr-index %))
         (filter
          (fn [[x, y]]
            (and
             (< -1 x rows)
             (< -1 y cols))))
         (map (partial index-1d input)))))

(defn matrix [m]
  {:values (flatten m) :rows (count m) :cols (count (first m))})

(defn edges-for [{:keys [values rows cols] :as input} [idx, value]]
  (->> (neighbors input idx)
       (filter #(< (nth values %) value))
       (map #(vector idx %))))

(defn build-edges [{:keys [values rows cols] :as input}]
  (->> (map-indexed vector values)
       (remove (fn [[idx, value]] (= value 9)))
       (map (partial edges-for input))
       (apply concat)))

(defn annotate-graph [{:keys [values]} graph]
  (reduce
   (fn [acc node] (add-attr acc node :value (nth values node)))
   graph
   (nodes graph)))

(defn build-graph [input]
  (->> (build-edges input)
       (apply digraph)
       (annotate-graph input)))

(defn parse-line [line]
  (->> (str/split line #"")
       (map #(Integer. %))))

(def input
  (->> (slurp "input.txt")
       str/split-lines
       (map parse-line)
       matrix
       build-graph))

(print (part1 input) (part2 input))
