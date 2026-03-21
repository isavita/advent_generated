
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s] (Integer/parseInt s))

(defn parse-line [line]
  (when-let [[_ d] (re-find #"^\[([.#]+)\]" line)]
    (let [target (mapv #(if (= % \#) 1 0) d)
          buttons (mapv (fn [[_ xs]]
                          (mapv parse-int (str/split xs #",")))
                        (re-seq #"\(([\d,]+)\)" line))]
      {:target target :buttons buttons})))

(defn xor-row [row pivot c]
  (let [n (count row)]
    (loop [i c
           r row]
      (if (= i n)
        r
        (recur (inc i)
               (assoc r i (bit-xor (nth r i) (nth pivot i))))))))

(defn gauss-min-weight [mat r c]
  (let [mat (transient (vec (mapv transient mat)))]
    (loop [pivot-row 0
           col 0
           pivots (transient (vec (repeat c false)))]
      (if (or (= col c) (= pivot-row r))
        (let [mat (persistent! (mapv persistent! (persistent! mat)))
              pivots (persistent! pivots)]
          (if (some #(= 1 (nth % c)) (subvec mat pivot-row r))
            -1
            (let [free-vars (vec (keep-indexed #(when (not %2) %1) pivots))
                  num-free (count free-vars)
                  limit (bit-shift-left 1 num-free)]
              (loop [mask 0
                     best Long/MAX_VALUE]
                (if (= mask limit)
                  best
                  (let [x (int-array c)
                        weight (atom 0)]
                    (dotimes [j num-free]
                      (when (bit-test mask j)
                        (aset-int x (free-vars j) 1)
                        (swap! weight inc)))
                    (loop [pr 0
                           cc 0]
                      (when (< cc c)
                        (when (nth pivots cc)
                          (let [val (reduce bit-xor
                                            (nth (nth mat pr) c)
                                            (for [k (range (inc cc) c)
                                                  :when (= 1 (nth (nth mat pr) k))]
                                              (aget x k)))]
                            (aset-int x cc val)
                            (when (= val 1) (swap! weight inc))
                            (recur (inc pr) (inc cc))))
                        (when (and (< cc c) (not (nth pivots cc)))
                          (recur pr (inc cc))))
                    (recur (inc mask) (min best @weight)))))))
        (let [sel (loop [rr pivot-row]
                    (cond
                      (= rr r) -1
                      (= 1 (nth (nth mat rr) col)) rr
                      :else (recur (inc rr))))]
          (if (= sel -1)
            (recur pivot-row (inc col) pivots)
            (do
              (let [tmp (nth mat pivot-row)]
                (assoc! mat pivot-row (nth mat sel))
                (assoc! mat sel tmp))
              (doseq [rr (range r)
                      :when (and (not= rr pivot-row)
                                 (= 1 (nth (nth mat rr) col)))]
                (assoc! mat rr (xor-row (nth mat rr) (nth mat pivot-row) col)))
              (assoc! pivots col true)
              (recur (inc pivot-row) (inc col) pivots))))))))

(defn solve-machine [{:keys [target buttons]}]
  (let [rows (count target)
        cols (count buttons)
        mat (vec (for [r (range rows)]
                   (vec (conj
                         (for [c (range cols)]
                           (reduce (fn [acc btn]
                                     (if (some #{r} btn) 1 0))
                                   0
                                   [nil]))
                         (nth target r)))))]
    (loop [r 0
           matrix (vec (for [rr (range rows)]
                         (vec (concat
                               (for [btn buttons]
                                 (if (some #{rr} btn) 1 0))
                               [(nth target rr)]))))]
      (gauss-min-weight matrix rows cols))))

(defn main []
  (let [machines (->> (slurp "input.txt")
                      str/split-lines
                      (keep parse-line))
        total (reduce + (keep solve-machine machines))]
    (println total)))

(main)
