(import (org.apache.commons.codec.binary Hex BinaryCodec))

(require '[clojure.string :as str])

(use 'blancas.kern.core
     'blancas.kern.i18n)

(defn bin->int [data]
  (Long/parseLong data 2))

(defn hex->bin [^String data]
  (->> data
       (.toCharArray)
       Hex/decodeHex
       reverse
       byte-array
       BinaryCodec/toAsciiString))

(def bin-digit
  (let [bin (set "01")]
    (<?> (satisfy (fn [^Character c] (bin c)))
         (di18n :bin-digit))))

(defn bin-seq [n]
  (<?> (>>= (<+> (times n bin-digit))
            return)
       (di18n :bin-seq)))

(defn bin-num [n]
  (<$> bin->int (bin-seq n)))

(declare packet)

(def packet-version
  (bin-num 3))

(defn packet-type [pred]
  (<$> bin->int pred))

(def literal-type
  (packet-type (token* "100")))

(def literal-seq
  (bind [header bin-digit]
        (if (= header \0)
          (bin-seq 4)
          (<+> (bin-seq 4) literal-seq))))

(def literal-num
  (<$> bin->int literal-seq))

(def parse-literal
  (<*> packet-version literal-type literal-num))

(def literal
  (bind [[packet-version packet-type packet-value] parse-literal]
        (return {:kind :literal
                 :version packet-version
                 :id packet-type
                 :value packet-value})))

(def operator-type
  (packet-type (bin-seq 3)))

(def operator-subpackets
  (bind [length-type-id bin-digit]
        (if (= length-type-id \0)
          (bind [subpacket-len (bin-num 15)]
                (<$> (partial value (many1 packet)) 
                     (bin-seq subpacket-len)))
          (bind [subpacket-amt (bin-num 11)]
                (times subpacket-amt packet)))))

(def parse-operator
  (<*> packet-version operator-type operator-subpackets))

(def operator
  (bind [[packet-version packet-type subpackets] parse-operator]
        (return {:kind :operator
                 :version packet-version
                 :id packet-type
                 :value subpackets})))

(def packet
  (<|> (<:> literal) (<:> operator)))

(defn part1 [{:keys [kind version value]}]
  (case kind
    :literal version
    :operator (->> value
                   (map part1)
                   (apply +)
                   (+ version))))

(defn get-fn-for-id [id]
  (partial apply
           (case id
             0 +
             1 *
             2 min
             3 max
             5 >
             6 <
             7 =)))

(defn coerce-long [x]
  (cond
    (boolean? x) ({true 1 false 0} x)
    :else (long x)))

(defn part2 [{:keys [kind id value]}]
  (case kind
    :literal value
    :operator (->> (map part2 value)
                   ((get-fn-for-id id))
                   coerce-long)))

(def input
  (->> (slurp "input.txt")
       str/trim
       hex->bin
       (value packet)))

(print (part1 input) (part2 input))
