
(ns some-assembly-required
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-int [s]
  (Long/parseUnsignedLong s))

(def max-u16 (dec (bit-shift-left 1 16)))

(defn parse-rule [line]
  (let [[rule wire] (str/split line #" -> ")]
    [wire rule]))

(defn build-graph [input]
  (into {} (map parse-rule) (str/split-lines input)))

(defn memo-dfs [graph wire memo]
  (if-let [cached (get memo wire)]
    [cached memo]
    (cond
      (re-matches #"\d+" wire)
      [(parse-int wire) memo]

      :else
      (let [rule (get graph wire)
            parts (str/split rule #" ")]
        (cond
          (= 1 (count parts))
          (let [[v m] (memo-dfs graph (first parts) memo)]
            [(bit-and v max-u16) (assoc m wire v)])

          (= "NOT" (first parts))
          (let [[v m] (memo-dfs graph (second parts) memo)
                v (bit-and (bit-xor max-u16 v) max-u16)]
            [v (assoc m wire v)])

          :else
          (let [[a op b] parts
                [va m1] (memo-dfs graph a memo)
                [vb m2] (memo-dfs graph b m1)
                v (case op
                    "AND"  (bit-and va vb)
                    "OR"   (bit-or va vb)
                    "LSHIFT" (bit-and (bit-shift-left va vb) max-u16)
                    "RSHIFT" (bit-shift-right va vb))]
            [v (assoc m2 wire v)]))))))

(defn some-assembly-required [input]
  (let [graph (build-graph input)
        [v _] (memo-dfs graph "a" {})]
    v))

(defn -main [& _]
  (println (some-assembly-required (str/trim (slurp "input.txt")))))

(-main)
