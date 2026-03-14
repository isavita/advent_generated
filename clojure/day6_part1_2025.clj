
(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.math BigInteger]))

(defn read-input []
  (with-open [rdr (io/reader "input.txt")]
    (vec (line-seq rdr))))

(defn is-separator? [lines col]
  (every? #(or (>= col (count %))
               (Character/isWhitespace (char (nth % col))))
          lines))

(defn parse-block [lines start-col end-col]
  (let [nums (atom [])
        op (atom nil)]
    (doseq [line lines]
      (when (< start-col (count line))
        (let [end (min (inc end-col) (count line))
              segment (subs line start-col end)
              trimmed (str/trim segment)]
          (when-not (str/blank? trimmed)
            (cond
              (= trimmed "+") (reset! op :add)
              (= trimmed "*") (reset! op :mul)
              :else (swap! nums conj (BigInteger. trimmed)))))))
    {:nums @nums :op @op}))

(defn process-block [grand-total block]
  (let [{:keys [nums op]} block
        result (cond
                 (= op :add) (reduce #(.add %1 %2) (BigInteger. "0") nums)
                 (= op :mul) (reduce #(.multiply %1 %2) (BigInteger. "1") nums)
                 (= (count nums) 1) (first nums)
                 :else (BigInteger. "0"))]
    (.add grand-total result)))

(defn solve []
  (let [lines (read-input)
        max-width (apply max (map count lines))
        padded-lines (mapv #(format (str "%-" max-width "s") %) lines)
        grand-total (atom (BigInteger. "0"))
        in-block (atom false)
        block-start (atom 0)]
    
    (doseq [col (range max-width)]
      (if (is-separator? padded-lines col)
        (when @in-block
          (let [block (parse-block padded-lines @block-start (dec col))]
            (when (seq (:nums block))
              (swap! grand-total process-block block))
            (reset! in-block false)))
        (when-not @in-block
          (reset! in-block true)
          (reset! block-start col))))
    
    (when @in-block
      (let [block (parse-block padded-lines @block-start (dec max-width))]
        (when (seq (:nums block))
          (swap! grand-total process-block block))))
    
    (println (str "Grand total: " @grand-total))))

(solve)
