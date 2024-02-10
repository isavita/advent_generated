(ns binary-diagnostic.core
  (:require [clojure.string :as str]))

(defn read-binary-input [filename]
  (slurp filename))

(defn parse-input [input]
  (str/split-lines input))

(defn most-common-bit [bits]
  (let [ones (count (filter #(= % \1) bits))
        zeros (count (filter #(= % \0) bits))]
    (if (>= ones zeros) \1 \0)))

(defn least-common-bit [bit]
  (if (= bit \1) \0 \1))

(defn calculate-rates [input]
  (let [lines (parse-input input)
        transposed (apply map vector lines)
        gamma-bits (map most-common-bit transposed)
        epsilon-bits (map least-common-bit gamma-bits)]
    [(apply str gamma-bits) (apply str epsilon-bits)]))

(defn binary-to-decimal [binary-str]
  (Integer/parseInt binary-str 2))

(defn calculate-power-consumption [filename]
  (let [input (read-binary-input filename)
        [gamma-str epsilon-str] (calculate-rates input)
        gamma (binary-to-decimal gamma-str)
        epsilon (binary-to-decimal epsilon-str)]
    (* gamma epsilon)))

(println (calculate-power-consumption "input.txt"))

