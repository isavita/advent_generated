(ns day7
  (:require [clojure.string :as str]))

(defn parse-instruction [instruction]
  (let [[_ input output] (re-matches #"(.+) -> (\w+)" instruction)]
    [output (read-string (str "(fn [circuit] " input ")"))]))

(defn build-circuit [instructions]
  (into {} (map parse-instruction instructions)))

(defn resolve-wire [circuit wire memo]
  (if-let [result (get @memo wire)]
    result
    (if-let [value (get circuit wire)]
      (if (number? value)
        (do (swap! memo assoc wire value) value)
        (let [result (value (fn [w] (resolve-wire circuit w memo)))]
          (swap! memo assoc wire result)
          result))
      (read-string wire))))

(defn solve [input]
  (let [circuit (build-circuit (str/split-lines input))
        memo (atom {})]
    (resolve-wire circuit "a" memo)))

; Helper functions for bitwise operations
(defn AND [x y] (bit-and x y))
(defn OR [x y] (bit-or x y))
(defn NOT [x] (bit-and 65535 (bit-not x)))
(defn LSHIFT [x n] (bit-and 65535 (bit-shift-left x n)))
(defn RSHIFT [x n] (bit-shift-right x n))
