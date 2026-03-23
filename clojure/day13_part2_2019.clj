
(ns solution
  (:require [clojure.string :as s]))

(defn exec [prog p2]
  (loop [m (if p2 (assoc prog 0 2) prog) ip 0 rb 0 bx 0 px 0 sc 0 bl 0 b []]
    (let [v (m ip 0) op (rem v 100)
          md #(rem (quot v (long (case (int %) 1 100 2 1000 3 10000))) 10)
          gv #(let [p (m (+ ip %) 0)] (case (int (md %)) 0 (m p 0) 1 p 2 (m (+ rb p) 0)))
          sv #(assoc m (let [p (m (+ ip %) 0)] (if (= (int (md %)) 2) (+ rb p) p)) %2)]
      (case (int op)
        99 (if p2 sc bl)
        1 (recur (sv 3 (+ (gv 1) (gv 2))) (+ ip 4) rb bx px sc bl b)
        2 (recur (sv 3 (* (gv 1) (gv 2))) (+ ip 4) rb bx px sc bl b)
        3 (recur (sv 1 (compare bx px)) (+ ip 2) rb bx px sc bl b)
        4 (let [nb (conj b (gv 1))]
            (if (= (count nb) 3)
              (let [[x y t] nb]
                (recur m (+ ip 2) rb (if (= t 4) x bx) (if (= t 3) x px) (if (and (= x -1) (= y 0)) t sc) (if (and (not p2) (= t 2)) (inc bl) bl) []))
              (recur m (+ ip 2) rb bx px sc bl nb)))
        5 (recur m (if (not= (gv 1) 0) (gv 2) (+ ip 3)) rb bx px sc bl b)
        6 (recur m (if (= (gv 1) 0) (gv 2) (+ ip 3)) rb bx px sc bl b)
        7 (recur (sv 3 (if (< (gv 1) (gv 2)) 1 0)) (+ ip 4) rb bx px sc bl b)
        8 (recur (sv 3 (if (= (gv 1) (gv 2)) 1 0)) (+ ip 4) rb bx px sc bl b)
        9 (recur m (+ ip 2) (+ rb (gv 1)) bx px sc bl b)))))

(let [p (-> (slurp "input.txt") s/trim (s/split #",") (->> (map #(Long/parseLong %)) (zipmap (range))))]
  (println "Part 1:" (exec p false))
  (println "Part 2:" (exec p true)))
