(require '[clojure.string :as str])

(defn add? [a b]
  (+ (or a 0) (or b 0)))

(defn step [insertions molecule]
  (reduce-kv (fn [acc [left right :as curr-key] curr-value]
               (let [curr (insertions curr-key)]
                 (-> acc
                     (update (str left curr) add? curr-value)
                     (update (str curr right) add? curr-value))))
             {} molecule))

(defn count-chars [[molecule] counter]
  (-> (reduce-kv (fn [acc [left right] curr-value]
                   (update acc left add? curr-value))
                 {} counter) 
      (update (last molecule) add? 1)))

(defn solve [[input counters] steps]
  (->> (nth counters steps)
       (count-chars input)
       vals
       ((juxt
         (partial apply max)
         (partial apply min)))
       (apply -)))

(defn zip [nums]
  (partition 2 (interleave nums (rest nums))))

(defn parse-molecule [[molecule]]
  molecule)

(defn parse-insertions [[molecule & insertions]]
  (->> (mapcat #(str/split % #" -> ") insertions)
       (apply hash-map)))

(defn iterate-input [[molecule insertions]]
  (->> molecule
       zip
       (map (partial str/join ""))
       frequencies
       (iterate (partial step insertions))))

(def input
  (->> (slurp "input.txt")
       str/split-lines
       (remove str/blank?)
       ((juxt parse-molecule parse-insertions))
       ((juxt identity iterate-input))))

(print (solve input 10) (solve input 40))
