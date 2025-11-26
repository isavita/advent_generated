
(ns arcade
  (:require [clojure.string :as str]))

(defn read-program [file]
  (->> (slurp file)
       str/trim
       (#(str/split % #","))
       (mapv parse-long)))

(defn run-program [program inputs]
  (let [mem (atom (vec program))
        rb (atom 0)
        ip (atom 0)
        in (atom (vec inputs))]
    (fn []
      (let [get-mem (fn [i] (if (< i (count @mem)) (@mem i) 0))
            set-mem (fn [i v]
                      (when (>= i (count @mem)) (swap! mem into (repeat (- i (count @mem) -1) 0)))
                      (swap! mem assoc i v))
            get-param (fn [mode off]
                        (let [v (get-mem (+ @ip off))]
                          (case (int mode)
                            0 (get-mem v)
                            1 v
                            2 (get-mem (+ v @rb)))))
            set-param (fn [mode off val]
                        (let [v (get-mem (+ @ip off))]
                          (case (int mode)
                            0 (set-mem v val)
                            2 (set-mem (+ v @rb) val))))]
        (loop []
          (let [inst (get-mem @ip)
                op (mod inst 100)
                m1 (mod (quot inst 100) 10)
                m2 (mod (quot inst 1000) 10)
                m3 (mod (quot inst 10000) 10)]
            (case op
              1 (do (set-param m3 3 (+ (get-param m1 1) (get-param m2 2)))
                    (swap! ip + 4)
                    (recur))
              2 (do (set-param m3 3 (* (get-param m1 1) (get-param m2 2)))
                    (swap! ip + 4)
                    (recur))
              3 (if (empty? @in)
                  ::input
                  (do (set-param m1 1 (first @in))
                      (swap! in subvec 1)
                      (swap! ip + 2)
                      (recur)))
              4 (let [out (get-param m1 1)]
                  (swap! ip + 2)
                  out)
              5 (if (not= (get-param m1 1) 0)
                  (do (reset! ip (get-param m2 2))
                      (recur))
                  (do (swap! ip + 3)
                      (recur)))
              6 (if (= (get-param m1 1) 0)
                  (do (reset! ip (get-param m2 2))
                      (recur))
                  (do (swap! ip + 3)
                      (recur)))
              7 (do (set-param m3 3 (if (< (get-param m1 1) (get-param m2 2)) 1 0))
                    (swap! ip + 4)
                    (recur))
              8 (do (set-param m3 3 (if (= (get-param m1 1) (get-param m2 2)) 1 0))
                    (swap! ip + 4)
                    (recur))
              9 (do (swap! rb + (get-param m1 1))
                    (swap! ip + 2)
                    (recur))
              99 nil
              nil)))))))

(defn count-blocks [program]
  (let [grid (atom {})
        cpu (run-program program [])
        step (fn step []
               (when-let [x (cpu)]
                 (if (= x ::input)
                   (recur)
                   (let [y (cpu)
                         t (cpu)]
                     (swap! grid assoc [x y] t)
                     (recur)))))]
    (step)
    (count (filter (fn [[_ v]] (= v 2)) @grid))))

(defn -main []
  (let [program (read-program "input.txt")]
    (println (count-blocks program))))

(-main)
