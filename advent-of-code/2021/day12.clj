(require '[clojure.string :as str]
         '[loom.graph :refer (digraph successors)])

(def start-node "start")
(def end-node "end")

(defn big? [node]
  (= node (str/upper-case node)))

(defn paths
  ([graph pred] (paths graph pred start-node []))
  ([graph pred node path]
    (if (= node end-node)
      (vector path)
      (->> (successors graph node)
           (filter #(pred % path))
           (map #(paths graph pred % (conj path %)))
           (apply concat)
           (into [])))))

(defn solve [graph pred]
  (->> (paths graph pred)
       count))

(defn part1 [node path]
  (or
   (big? node)
   (not
    (some #(= % node) path))))

(defn part2 [node path]
  (or
   (part1 node path)
   (->> (remove big? path)
        frequencies
        (map second)
        (apply max)
        (= 1))))

(defn parse-line [line]
  (str/split line #"-"))

(defn invert [lines]
  (map #(into [] (reverse %)) lines))

(defn edges [lines]
  (->> lines
       ((juxt identity invert))
       (apply concat)
       (remove
        (fn [[start end]]
          (or
           (= start end-node)
           (= end start-node))))))

(def input
  (->> (slurp "input.txt")
       str/split-lines
       (map parse-line)
       edges
       (apply digraph)))

(print (solve input part1) (solve input part2))
