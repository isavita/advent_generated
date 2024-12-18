
(ns day15
  (:require [clojure.string :as str]))

(defn hash-algorithm [s]
  (reduce (fn [acc c]
            (-> acc
                (+ (int c))
                (* 17)
                (rem 256)))
          0
          s))

(defn solve-part1 [input]
  (->> (str/split input #",")
       (map hash-algorithm)
       (reduce +)))

(defn solve-part2 [input]
  (let [steps (str/split input #",")
        boxes (vec (repeat 256 []))]
    (loop [steps steps
           boxes boxes]
      (if (empty? steps)
        (reduce-kv (fn [acc box-num box]
                     (+ acc (reduce-kv (fn [acc2 slot-num [_ focal-length]]
                                         (+ acc2 (* (inc box-num) (inc slot-num) focal-length)))
                                       0
                                       box)))
                   0
                   boxes)
        (let [step (first steps)
              [label op focal-length] (if (str/includes? step "=")
                                        (let [[label focal-length] (str/split step #"=")]
                                          [label "=" (parse-long focal-length)])
                                        [(subs step 0 (dec (count step))) "-" nil])
              box-num (hash-algorithm label)
              box (boxes box-num)]
          (recur (rest steps)
                 (assoc boxes box-num
                        (case op
                          "=" (if-let [idx (first (keep-indexed #(when (= (first %2) label) %1) box))]
                                (assoc box idx [label focal-length])
                                (conj box [label focal-length]))
                          "-" (filterv #(not= (first %) label) box)))))))))

(let [input (slurp "input.txt")]
  (println "Part 1:" (solve-part1 input))
  (println "Part 2:" (solve-part2 input)))
