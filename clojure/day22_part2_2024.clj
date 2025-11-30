
(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def mod-val (bit-shift-left 1 24))
(def num-steps 2000)

(defn next-secret [s]
  (let [x1 (* s 64)
        r1 (bit-and (bit-xor s x1) (dec mod-val))
        x2 (quot r1 32)
        r2 (bit-and (bit-xor r1 x2) (dec mod-val))
        x3 (* r2 2048)]
    (bit-and (bit-xor r2 x3) (dec mod-val))))

(defn encode-change4 [c1 c2 c3 c4]
  (+ (+ 9 c1)
     (* 19 (+ 9 c2))
     (* 19 19 (+ 9 c3))
     (* 19 19 19 (+ 9 c4))))

(defn process-buyer [init-val]
  (let [prices (long-array (inc num-steps))
        changes (int-array num-steps)]
    (loop [s init-val, j 0]
      (aset prices j (mod s 10))
      (if (< j num-steps)
        (recur (next-secret s) (inc j))))
    (dotimes [j num-steps]
      (aset changes j (- (aget prices (inc j)) (aget prices j))))
    [prices changes]))

(defn -main [& _]
  (let [initials (->> (slurp "input.txt")
                      str/split-lines
                      (remove str/blank?)
                      (mapv #(Long/parseLong %)))
        buyers (mapv process-buyer initials)
        pattern-count (* 19 19 19 19)
        global-sum (long-array pattern-count)]
    (doseq [[prices changes] buyers]
      (let [local-price (int-array pattern-count -1)]
        (dotimes [i (- num-steps 3)]
          (let [c1 (aget changes i)
                c2 (aget changes (inc i))
                c3 (aget changes (+ 2 i))
                c4 (aget changes (+ 3 i))]
            (when (and (<= -9 c1 9) (<= -9 c2 9) (<= -9 c3 9) (<= -9 c4 9))
              (let [idx (encode-change4 c1 c2 c3 c4)]
                (when (< (aget local-price idx) 0)
                  (aset local-price idx (aget prices (+ 4 i))))))))
        (dotimes [idx pattern-count]
          (when (>= (aget local-price idx) 0)
            (aset global-sum idx (+ (aget global-sum idx) (aget local-price idx)))))))
    (println (apply max (seq global-sum)))))

(-main)
