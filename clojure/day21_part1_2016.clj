
(ns scramble.core
  (:require [clojure.string :as str]))

(defn swap-pos [s x y]
  (let [v (vec s)]
    (str/join (assoc v x (v y) y (v x)))))

(defn swap-letter [s x y]
  (-> s
      (str/replace x "_")
      (str/replace y x)
      (str/replace "_" y)))

(defn rotate-left [s steps]
  (let [n (mod steps (count s))]
    (str (subs s n) (subs s 0 n))))

(defn rotate-right [s steps]
  (let [n (mod steps (count s))]
    (str (subs s (- (count s) n)) (subs s 0 (- (count s) n)))))

(defn rotate-based [s x]
  (let [idx (str/index-of s x)
        steps (+ 1 idx (if (>= idx 4) 1 0))]
    (rotate-right s steps)))

(defn reverse-range [s x y]
  (let [prefix (subs s 0 x)
        middle (str/reverse (subs s x (inc y)))
        suffix (subs s (inc y))]
    (str prefix middle suffix)))

(defn move-pos [s x y]
  (let [c (nth s x)
        without (str (subs s 0 x) (subs s (inc x)))]
    (str (subs without 0 y) c (subs without y))))

(defn apply-op [password op]
  (let [parts (str/split op #" ")]
    (case (first parts)
      "swap" (if (= (second parts) "position")
               (swap-pos password (Long/parseUnsignedLong (nth parts 2))
                                 (Long/parseUnsignedLong (nth parts 5)))
               (swap-letter password (nth parts 2) (nth parts 5)))
      "rotate" (case (second parts)
                 "left" (rotate-left password (Long/parseUnsignedLong (nth parts 2)))
                 "right" (rotate-right password (Long/parseUnsignedLong (nth parts 2)))
                 "based" (rotate-based password (nth parts 6)))
      "reverse" (reverse-range password (Long/parseUnsignedLong (nth parts 2))
                                           (Long/parseUnsignedLong (nth parts 4)))
      "move" (move-pos password (Long/parseUnsignedLong (nth parts 2))
                               (Long/parseUnsignedLong (nth parts 5))))))

(defn -main [& _]
  (let [ops (-> (slurp "input.txt")
                (str/trim)
                (str/split-lines))]
    (println (reduce apply-op "abcdefgh" ops))))

(-main)
