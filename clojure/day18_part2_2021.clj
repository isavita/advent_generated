
(ns snailfish
  (:require [clojure.string :as str]))

(defn parse-number [s]
  (read-string (str/replace s #"\[" "["))) ; Convert to Clojure vectors

(defn explode [num]
  (letfn [(find-explode [n depth]
            (cond
              (and (vector? n) (= depth 4)) [true (first n) (second n) 0]
              (vector? n)
              (let [[l r] n
                    [found? lv rv new-l] (find-explode l (inc depth))]
                (if found?
                  (if (pos? rv)
                    [true lv 0 [new-l (add-leftmost r rv)]]
                    [true lv 0 [new-l r]])
                  (let [[found? lv rv new-r] (find-explode r (inc depth))]
                    (if found?
                      (if (pos? lv)
                        [true 0 rv [(add-rightmost l lv) new-r]]
                        [true 0 rv [l new-r]])
                      [false 0 0 n]))))
              :else [false 0 0 n]))
          (add-leftmost [n v]
            (if (number? n) (+ n v)
                [(add-leftmost (first n) v) (second n)]))
          (add-rightmost [n v]
            (if (number? n) (+ n v)
                [(first n) (add-rightmost (second n) v)]))]
    (find-explode num 0)))

(defn split-num [n]
  (if (number? n)
    (if (>= n 10) [true [(quot n 2) (quot (inc n) 2)]] [false n])
    (let [[l r] n]
      (let [[split? new-l] (split-num l)]
        (if split?
          [true [new-l r]]
          (let [[split? new-r] (split-num r)]
            (if split?
              [true [l new-r]]
              [false n])))))))

(defn reduce-num [num]
  (loop [curr num]
    (let [[found? _ _ new-n] (explode curr)]
      (if found?
        (recur new-n)
        (let [[split? new-n2] (split-num curr)]
          (if split? (recur new-n2) curr))))))

(defn add [a b]
  (reduce-num [a b]))

(defn magnitude [n]
  (if (number? n) n
      (+ (* 3 (magnitude (first n))) (* 2 (magnitude (second n))))))

(defn solve []
  (let [nums (->> "input.txt"
                  slurp
                  str/split-lines
                  (map parse-number))]
    (apply max
           (for [i (range (count nums))
                 j (range (count nums))
                 :when (not= i j)]
             (max (magnitude (add (nth nums i) (nth nums j)))
                  (magnitude (add (nth nums j) (nth nums i))))))))

(println (solve))
