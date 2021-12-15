(require '[clojure.string :as str]
         '[loom.graph :refer (weighted-digraph nodes)]
         '[loom.alg :refer (dijkstra-path-dist)])

(load-file "./matrix-utils.clj")

(defn build-risk-map-for-row [input risk-fn row]
  (->> (range)
       (map (partial vector row))
       (map (juxt identity (partial risk-fn input)))
       (take-while (fn [[pos risk-value]] (some? risk-value)))))

(defn build-risk-map [input risk-fn]
  (->> (range)
       (map (partial build-risk-map-for-row input risk-fn))
       (take-while seq)
       (apply concat)
       (into {})))

(defn edges-for [risk-map [x y :as pos]]
  (->> deltas-no-diagonals
       (map (partial mapv + pos))
       (map (juxt identity (partial get risk-map)))
       (remove (fn [[pos risk-value]] (nil? risk-value)))
       (map (partial concat (list pos)))
       (map (partial into []))))

(defn build-graph [input risk-fn]
  (let [risk-map (build-risk-map input risk-fn)]
    (->> (keys risk-map)
         (mapcat (partial edges-for risk-map))
         (apply weighted-digraph))))

(defn solve [input risk-fn]
  (let [graph (build-graph input risk-fn)
        start-node [0 0]
        end-node (last (sort (nodes graph)))]
    (->> (dijkstra-path-dist graph start-node end-node)
         second)))

(defn part1 [{:keys [values]} idx]
  (get values idx))

(defn part2 [{:keys [rows cols] :as input} [x y :as idx]]
  (when (and
         (< x (* rows 5))
         (< y (* cols 5)))
    (-> (part1 input [(rem x rows), (rem y cols)])
        (+ (quot x rows))
        (+ (quot y cols))
        dec
        (rem 9)
        inc)))

(defn mat->map [mat]
  (let [rows (count mat)
        cols (count (first mat))]
    {:rows rows :cols cols
     :values (into {} (for [[i row] (map-indexed vector mat)
                            [j col] (map-indexed vector row)]
                        [(vector i j) col]))}))

(defn parse-line [line]
  (->> (str/split line #"")
       (map #(Integer. %))))

(def input
  (->> (slurp "input.txt")
       str/split-lines
       (map parse-line)
       mat->map))

(print (solve input part1) (solve input part2))
