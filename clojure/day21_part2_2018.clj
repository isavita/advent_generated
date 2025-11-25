
(ns main
  (:require [clojure.string :as str]))

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

(defn parse-input [txt]
  (let [lines (str/split-lines txt)
        ip (Long/parseLong (second (re-matches #"#ip (\d+)" (first lines))))
        instrs (mapv (fn [l]
                       (let [[op & nums] (str/split l #"\s+")]
                         [(symbol op) (mapv #(Long/parseLong %) nums)]))
                     (rest lines))]
    [ip instrs]))

(defn run [ip instrs]
  (let [regs (vec (repeat 6 0))
        seen (atom #{})
        last5 (atom nil)]
    (loop [regs regs]
      (let [idx (regs ip)]
        (when (< idx (count instrs))
          (let [[op [a b c]] (instrs idx)
                regs ((ops op) regs a b c)
                regs (update regs ip inc)]
            (if (= (regs ip) 28)
              (let [r5 (regs 5)]
                (if (contains? @seen r5)
                  @last5
                  (do (swap! seen conj r5)
                      (reset! last5 r5)
                      (recur regs))))
              (recur regs))))))))

(defn -main [& _]
  (let [[ip instrs] (parse-input (slurp "input.txt"))]
    (println (run ip instrs))))

(-main)
