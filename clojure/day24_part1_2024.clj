
(ns main
  (:require [clojure.string :as str]))

(defn parse-wire [line]
  (when-let [[_ name val-str] (re-matches #"(\w+): (\d+)" line)]
    [(keyword name) (Integer/parseInt val-str)]))

(defn parse-gate [line]
  (when-let [[_ in1 op in2 out] (re-matches #"(\w+) (AND|OR|XOR) (\w+) -> (\w+)" line)]
    {:in1 (keyword in1) :in2 (keyword in2) :out (keyword out) :op (keyword op)}))

(defn apply-op [op v1 v2]
  (case op
    :AND (bit-and v1 v2)
    :OR  (bit-or v1 v2)
    :XOR (bit-xor v1 v2)))

(defn simulate [gates wires]
  (loop [wires wires]
    (if-let [{:keys [in1 in2 out op] :as gate}
             (first (filter #(and (contains? wires (:in1 %))
                                   (contains? wires (:in2 %))
                                   (not (contains? wires (:out %)))) gates))]
      (recur (assoc wires out (apply-op op (wires in1) (wires in2))))
      wires)))

(defn z-bits->long [wires]
  (let [z-wires (filter #(str/starts-with? (name (key %)) "z") wires)]
    (reduce (fn [acc [wire val]]
              (if (pos? val)
                (bit-set acc (Integer/parseInt (subs (name wire) 1)))
                acc))
            0
            (map (fn [[wire val]] [wire val]) z-wires))))

(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))
        wire-lines (take-while #(not (empty? %)) lines)
        gate-lines (drop (inc (count wire-lines)) lines)
        wires (into {} (keep parse-wire wire-lines))
        gates (keep parse-gate gate-lines)]
    (-> (simulate gates wires)
        z-bits->long
        println)))

(-main)
