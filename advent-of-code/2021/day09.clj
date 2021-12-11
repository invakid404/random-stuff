(require '[clojure.string :as str]
         '[loom.graph :refer (nodes digraph out-degree)]
         '[loom.attr :refer (add-attr attr)]
         '[loom.alg :refer (connected-components)])

(load-file "./matrix-utils.clj")

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

(defn edges-for [{:keys [values rows cols] :as input} [idx, value]]
  (->> (neighbors deltas-no-diagonals input idx)
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
