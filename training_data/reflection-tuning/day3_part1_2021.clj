(ns day3.core
  (:require [clojure.string :as str]))

(defn most-common-bit [freqs]
  (if (>= (get freqs \1 0) (get freqs \0 0)) \1 \0))

(defn least-common-bit [freqs]
  (if (< (get freqs \1 0) (get freqs \0 0)) \1 \0))

(defn binary-to-decimal [binary-str]
  (Integer/parseInt binary-str 2))

(defn calculate-power-consumption [input]
  (let [bit-columns (apply map vector input)
        bit-frequencies (map frequencies bit-columns)
        gamma-bits (map most-common-bit bit-frequencies)
        epsilon-bits (map least-common-bit bit-frequencies)
        gamma-rate (binary-to-decimal (str/join gamma-bits))
        epsilon-rate (binary-to-decimal (str/join epsilon-bits))]
    (* gamma-rate epsilon-rate)))

(defn -main []
  (let [input (str/split-lines (slurp "input.txt"))
        power-consumption (calculate-power-consumption input)]
    (println power-consumption)))

(-main)
