
(ns bridge-repair
  (:require [clojure.string :as str]))

(defn operate [op x y]
  (case op
    :+ (+ x y)
    :* (* x y)
    :|| (parse-long (str x y))))

(defn solve-equation [target nums]
  (let [n (count nums)]
    (if (= n 1)
      (= target (first nums))
      (let [ops (repeatedly (dec n) #(rand-nth [:+ :* :||]))]
        (loop [i 0
               acc (first nums)
               ops-cycle (cycle [:+ :* :||])]
          (if (= i (dec n))
            (= target acc)
              (let [next-num (nth nums (inc i))
                    op-combinations (take (dec n) (for [x (repeatedly (dec n) #(rand-nth [:+ :* :||]))] x))]
                (some
                  (fn [ops]
                    (= target (reduce (fn [a [op b]] (operate op a b))
                                  (first nums) (map vector ops (rest nums)))))
                  op-combinations)
                )
            )
          )))))



(defn solve-equation-recursive [target nums]
    (letfn [(evaluate [nums ops]
              (reduce (fn [acc [op num]]
                        (operate op acc num))
                      (first nums)
                      (map vector ops (rest nums))))
            (generate-ops [remaining]
              (if (zero? remaining)
                '(())
                (for [op [:+ :* :||]
                      more-ops (generate-ops (dec remaining))]
                  (cons op more-ops))))]
      (if (= (count nums) 1)
          (= target (first nums))
          (some #(= target (evaluate nums %)) (generate-ops (dec (count nums)))))))


(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))
        equations (map #(str/split % #": ") lines)
        results (pmap (fn [[target-str nums-str]]
                       (let [target (parse-long target-str)
                             nums (map parse-long (str/split nums-str #" "))]
                         (if (solve-equation-recursive target nums)
                           target
                           0)))
                     equations)
        total-calibration-result (reduce + results)]
        (println "Total calibration result:" total-calibration-result)
    )
  )

(-main)
