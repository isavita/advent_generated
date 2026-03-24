
(ns solution
  (:require [clojure.string :as str]))

(defn get-key [a op b]
  (let [[s1 s2] (sort [a b])]
    (str s1 "_" op "_" s2)))

(defn create-lookups [gates]
  [(into {} (map (juxt :out identity) gates))
   (into {} (map (fn [g] [(get-key (:a g) (:op g) (:b g)) (:out g)]) gates))])

(defn swap-wires [gates a b]
  (mapv (fn [g]
          (cond (= (:out g) a) (assoc g :out b)
                (= (:out g) b) (assoc g :out a)
                :else g))
        gates))

(defn solve []
  (let [input (slurp "input.txt")
        parts (str/split input #"\n\n")
        gate-lines (str/split-lines (second parts))
        gates (keep (fn [line]
                      (let [p (str/split (str/trim line) #"\s+")]
                        (when (= (count p) 5)
                          {:a (nth p 0) :op (nth p 1) :b (nth p 2) :out (nth p 4)})))
                    gate-lines)
        num-z (count (filter #(str/starts-with? (:out %) "z") gates))]
    (loop [curr-gates gates
           pairs []]
      (let [[lookup rev] (create-lookups curr-gates)
            swap (loop [i 0 carry-in ""]
                   (if (< i num-z)
                     (let [si (format "%02d" i)
                           xi (str "x" si) yi (str "y" si) zi (str "z" si)
                           bit (get rev (get-key xi "XOR" yi))]
                       (if (= i 0)
                         (let [co (get rev (get-key xi "AND" yi))]
                           (if (and bit (not= bit zi)) [bit zi] (recur (inc i) co)))
                         (let [fa (when (and bit (seq carry-in))
                                    (get rev (get-key bit "XOR" carry-in)))]
                           (cond
                             (and (not fa) (seq carry-in) (get lookup zi))
                             (let [gz (get lookup zi)]
                               (cond
                                 (and bit (get rev (get-key (:a gz) "XOR" carry-in))) [bit (:a gz)]
                                 (and bit (get rev (get-key (:b gz) "XOR" carry-in))) [bit (:b gz)]
                                 :else nil))
                             (and fa (not= fa zi)) [fa zi]
                             :else
                             (let [c1 (get rev (get-key xi "AND" yi))
                                   c2 (when (and bit (seq carry-in))
                                        (get rev (get-key bit "AND" carry-in)))
                                   co (if (and c1 c2)
                                        (get rev (get-key c1 "OR" c2))
                                        "")]
                               (recur (inc i) co))))))
                     nil))]
        (if (and swap (< (count pairs) 8))
          (recur (apply swap-wires curr-gates swap) (into pairs swap))
          (println (str/join "," (sort pairs))))))))

(solve)
