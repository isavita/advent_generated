
(ns main
  (:require [clojure.string :as str])
  (:import [java.util ArrayDeque HashMap]))

(def k1-layout
  {[1 1] \7, [1 2] \8, [1 3] \9
   [2 1] \4, [2 2] \5, [2 3] \6
   [3 1] \1, [3 2] \2, [3 3] \3
   [4 2] \0, [4 3] \A})

(def k1-coords
  {\7 [1 1], \8 [1 2], \9 [1 3]
   \4 [2 1], \5 [2 2], \6 [2 3]
   \1 [3 1], \2 [3 2], \3 [3 3]
   \0 [4 2], \A [4 3]})

(def k23-layout
  {[1 2] \^, [1 3] \A
   [2 1] \<, [2 2] \v, [2 3] \>})

(def k23-coords
  {\^ [1 2], \A [1 3], \< [2 1], \v [2 2], \> [2 3]})

(def moves {\^ [-1 0], \v [1 0], \< [0 -1], \> [0 1]})

(defn valid-k1? [p] (contains? k1-layout p))
(defn valid-k23? [p] (contains? k23-layout p))

(defn try-move [[r c] m]
  (let [[dr dc] (moves m)] [(+ r dr) (+ c dc)]))

(defn next-state [[p1 p2 p3] m3]
  (if (moves m3)
    (let [p3n (try-move p3 m3)]
      (if (valid-k23? p3n)
        [[p1 p2 p3n] nil false]
        [[p1 p2 p3] nil true]))
    (let [b3 (k23-layout p3)]
      (if (moves b3)
        (let [p2n (try-move p2 b3)]
          (if (valid-k23? p2n)
            [[p1 p2n p3] nil false]
            [[p1 p2 p3] nil true]))
        (let [b2 (k23-layout p2)]
          (if (moves b2)
            (let [p1n (try-move p1 b2)]
              (if (valid-k1? p1n)
                [[p1n p2 p3] nil false]
                [[p1 p2 p3] nil true]))
            [[p1 p2 p3] (k1-layout p1) false]))))))

(defn code-value [s]
  (let [digits (apply str (filter #(Character/isDigit ^char %) s))]
    (Long/parseLong (if (seq digits) digits "0"))))

(defn solve-code [code]
  (let [target (vec code)
        n (count target)
        start [(k1-coords \A) (k23-coords \A) (k23-coords \A) 0]
        q (ArrayDeque.)
        dist (HashMap.)]
    (.add q [start 0])
    (.put dist start 0)
    (loop []
      (if (.isEmpty q)
        -1
        (let [[[p1 p2 p3 typed] d] (.removeFirst q)]
          (if (= typed n)
            d
            (do
              (doseq [m [\< \> \^ \v \A]]
                (let [[[n1 n2 n3] out panic] (next-state [p1 p2 p3] m)]
                  (when-not panic
                    (let [typed2 (if out
                                   (if (and (< typed n) (= out (nth target typed)))
                                     (inc typed)
                                     -1)
                                   typed)]
                      (when (not= typed2 -1)
                        (let [s [n1 n2 n3 typed2]
                              nd (inc d)
                              old (.get dist s)]
                          (when (or (nil? old) (< nd old))
                            (.put dist s nd)
                            (.add q [s nd]))))))))
              (recur))))))))

(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))
        total (reduce
                (fn [acc code]
                  (let [v (code-value code)
                        d (solve-code code)]
                    (+ acc (* d v))))
                0
                lines)]
    (println total)))

(-main)
