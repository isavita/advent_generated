
(ns solution
  (:require [clojure.string :as str]))

(defn run [mem in-data]
  (loop [m mem ip 0 rb 0 in in-data out []]
    (let [instr (get m ip 0) op (mod instr 100)
          m1 (mod (quot instr 100) 10) m2 (mod (quot instr 1000) 10) m3 (mod (quot instr 10000) 10)
          gv (fn [off mo] (let [v (get m (+ ip off) 0)] (case (int mo) 0 (get m v 0) 1 v 2 (get m (+ rb v) 0))))
          sv (fn [off mo v] (let [addr (get m (+ ip off) 0)] (case (int mo) 0 (assoc m addr v) 2 (assoc m (+ rb addr) v))))]
      (case (int op)
        1 (recur (sv 3 m3 (+ (gv 1 m1) (gv 2 m2))) (+ ip 4) rb in out)
        2 (recur (sv 3 m3 (* (gv 1 m1) (gv 2 m2))) (+ ip 4) rb in out)
        3 (recur (sv 1 m1 (first in)) (+ ip 2) rb (rest in) out)
        4 (recur m (+ ip 2) rb in (conj out (gv 1 m1)))
        5 (recur m (if (not= 0 (gv 1 m1)) (gv 2 m2) (+ ip 3)) rb in out)
        6 (recur m (if (= 0 (gv 1 m1)) (gv 2 m2) (+ ip 3)) rb in out)
        7 (recur (sv 3 m3 (if (< (gv 1 m1) (gv 2 m2)) 1 0)) (+ ip 4) rb in out)
        8 (recur (sv 3 m3 (if (= (gv 1 m1) (gv 2 m2)) 1 0)) (+ ip 4) rb in out)
        9 (recur m (+ ip 2) (+ rb (gv 1 m1)) in out)
        99 out))))

(defn get-next [parts a b]
  (loop [p parts]
    (cond (empty? p) nil
          (= (take (count a) p) a) (recur (drop (count a) p))
          (and b (= (take (count b) p) b)) (recur (drop (count b) p))
          :else p)))

(defn check [parts a b c]
  (loop [p parts res []]
    (cond (empty? p) (str/join "," res)
          (= (take (count a) p) a) (recur (drop (count a) p) (conj res "A"))
          (= (take (count b) p) b) (recur (drop (count b) p) (conj res "B"))
          (= (take (count c) p) c) (recur (drop (count c) p) (conj res "C"))
          :else nil)))

(defn find-abc [path]
  (first
    (for [al (range 2 11 2) :let [ap (take al path) a (str/join "," ap)] :when (<= (count a) 20)
          :let [ra (get-next path ap nil)] :when ra
          bl (range 2 11 2) :let [bp (take bl ra) b (str/join "," bp)] :when (and (seq bp) (<= (count b) 20))
          :let [rab (get-next path ap bp)] :when rab
          cl (range 2 11 2) :let [cp (take cl rab) c (str/join "," cp)] :when (and (seq cp) (<= (count c) 20))
          :let [main (check path ap bp cp)] :when (and main (<= (count main) 20))]
      [main a b c])))

(let [prog (mapv parse-long (str/split (str/trim (slurp "input.txt")) #","))
      mem (into {} (map-indexed vector prog))
      out1 (run mem [])
      ls (str/split-lines (apply str (map char out1)))
      grid (into {} (for [y (range (count ls)) x (range (count (ls y)))] [[x y] (nth (ls y) x)]))
      [sp sv] (first (filter #(contains? #{\^ \v \< \>} (second %)) grid))
      sd (case sv \^ [0 -1] \v [0 1] \< [-1 0] \> [1 0])
      path (loop [p sp d sd dist 0 res []]
             (let [np (mapv + p d)]
               (if (= (grid np) \#) (recur np d (inc dist) res)
                 (let [dr [(- (d 1)) (d 0)] dl [(d 1) (- (d 0))]
                       r (if (pos? dist) (conj res dist) res)]
                   (cond (= (grid (mapv + p dr)) \#) (recur p dr 0 (conj r "R"))
                         (= (grid (mapv + p dl)) \#) (recur p dl 0 (conj r "L"))
                         :else r)))))
      [main a b c] (find-abc path)
      in (map int (str/join "\n" [main a b c "n" ""]))
      out2 (run (assoc mem 0 2) in)]
  (println (last out2)))
