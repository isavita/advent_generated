
(ns main
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def ops
  {'addr (fn [r a b c] (assoc r c (+ (r a) (r b))))
   'addi (fn [r a b c] (assoc r c (+ (r a) b)))
   'mulr (fn [r a b c] (assoc r c (* (r a) (r b))))
   'muli (fn [r a b c] (assoc r c (* (r a) b)))
   'banr (fn [r a b c] (assoc r c (bit-and (r a) (r b))))
   'bani (fn [r a b c] (assoc r c (bit-and (r a) b)))
   'borr (fn [r a b c] (assoc r c (bit-or (r a) (r b))))
   'bori (fn [r a b c] (assoc r c (bit-or (r a) b)))
   'setr (fn [r a _ c] (assoc r c (r a)))
   'seti (fn [r a _ c] (assoc r c a))
   'gtir (fn [r a b c] (assoc r c (if (> a (r b)) 1 0)))
   'gtri (fn [r a b c] (assoc r c (if (> (r a) b) 1 0)))
   'gtrr (fn [r a b c] (assoc r c (if (> (r a) (r b)) 1 0)))
   'eqir (fn [r a b c] (assoc r c (if (= a (r b)) 1 0)))
   'eqri (fn [r a b c] (assoc r c (if (= (r a) b) 1 0)))
   'eqrr (fn [r a b c] (assoc r c (if (= (r a) (r b)) 1 0)))})

(defn parse-regs [s]
  (when-let [m (re-matches #".*\[(\d+), (\d+), (\d+), (\d+)\].*" s)]
    (mapv #(Long/parseLong %) (rest m))))

(defn parse [lines]
  (loop [lines lines samples []]
    (if-let [before (and (seq lines) (parse-regs (first lines)))]
      (let [inst (mapv #(Long/parseLong %) (str/split (second lines) #"\s+"))
            after (parse-regs (nth lines 2))]
        (recur (drop 4 lines) (conj samples {:before before :inst inst :after after})))
      samples)))

(defn matches [sample]
  (let [a (nth (:inst sample) 1)
        b (nth (:inst sample) 2)
        c (nth (:inst sample) 3)
        before (:before sample)
        after (:after sample)]
    (count (filter #(= (apply % [before a b c]) after) (vals ops)))))

(defn -main [& _]
  (let [samples (parse (line-seq (io/reader "input.txt")))]
    (println (count (filter #(>= (matches %) 3) samples)))))

(-main)
