
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line]
  (mapv #(Long/parseLong %) (re-seq #"-?\d+" line)))

(defn apply-gravity [moons axis]
  (let [n (count moons)]
    (reduce (fn [moons [i j]]
              (let [pi (get-in moons [i 0 axis])
                    pj (get-in moons [j 0 axis])
                    cmp (compare pi pj)
                    delta (if (pos? cmp) -1 (if (neg? cmp) 1 0))]
                (-> moons
                    (update-in [i 1 axis] + delta)
                    (update-in [j 1 axis] - delta))))
            moons
            (for [i (range n) j (range (inc i) n)] [i j]))))

(defn apply-velocity [moons axis]
  (mapv (fn [[pos vel]]
          [(update pos axis + (get vel axis)) vel])
        moons))

(defn find-cycle [moons axis]
  (let [initial (mapv (fn [[pos vel]]
                        [(get pos axis) (get vel axis)])
                      moons)]
    (loop [moons moons steps 1]
      (let [moons (-> moons (apply-gravity axis) (apply-velocity axis))
            current (mapv (fn [[pos vel]]
                          [(get pos axis) (get vel axis)])
                        moons)]
        (if (= current initial)
          steps
          (recur moons (inc steps)))))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))
        moons (mapv (fn [line]
                      (let [pos (parse-line line)]
                        [pos [0 0 0]]))
                    lines)
        cycle-x (find-cycle moons 0)
        cycle-y (find-cycle moons 1)
        cycle-z (find-cycle moons 2)]
    (println (reduce lcm [cycle-x cycle-y cycle-z]))))

(-main)
