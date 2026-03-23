
(ns solution
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn solve []
  (let [lines (s/split-lines (slurp "input.txt"))
        clay (into #{} (for [l lines
                             :let [[_ t v1 _ v2 v3] (re-matches #"([xy])=(\d+), ([xy])=(\d+)\.\.(\d+)" l)
                                   [a b c] (map #(Integer/parseInt %) [v1 v2 v3])]
                             i (range b (inc c))]
                         (if (= t "x") [a i] [i a])))
        [min-y max-y] ((juxt #(apply min %) #(apply max %)) (map second clay))
        settled (atom #{})
        flowing (atom #{})
        stack (doto (java.util.Stack.) (.push [500 0]))
        seen (atom #{})]
    (while (not (.isEmpty stack))
      (let [[x y] (.pop stack)]
        (when-not (or (> y max-y) (@seen [x y]))
          (swap! seen conj [x y])
          (loop [cy y]
            (when (<= cy max-y)
              (swap! flowing conj [x cy])
              (let [below [x (inc cy)]]
                (cond
                  (or (clay below) (@settled below))
                  (loop [sy cy]
                    (let [scan (fn [dx] (loop [cx x] (if (clay [cx sy]) [:clay cx]
                                                       (if-not (or (clay [cx (inc sy)]) (@settled [cx (inc sy)])) [:hole cx] (recur (+ cx dx))))))
                          [lt lx] (scan -1) [rt rx] (scan 1)]
                      (if (and (= lt :clay) (= rt :clay))
                        (do (doseq [fx (range (inc lx) rx)] (swap! settled conj [fx sy]))
                            (recur (dec sy)))
                        (do (let [start (if (= lt :clay) (inc lx) lx)
                                  end   (if (= rt :clay) rx (inc rx))]
                              (doseq [fx (range start end)] (swap! flowing conj [fx sy])))
                            (if (= lt :hole) (.push stack [lx sy]))
                            (if (= rt :hole) (.push stack [rx sy]))))))
                  (@flowing below) nil
                  :else (recur (inc cy)))))))))
    (println (count (filter (fn [[_ y]] (<= min-y y max-y)) (set/union @settled @flowing))))))

(solve)
