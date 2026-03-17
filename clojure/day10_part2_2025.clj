
(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-line [line]
  (let [btn-matches (re-seq #"\(([^)]*)\)" line)
        buttons (mapv (fn [[_ inside]]
                        (if (str/blank? inside)
                          []
                          (mapv #(Integer/parseInt (str/trim %))
                                (str/split inside #","))))
                      btn-matches)
        tgt-match (re-find #"\{([^}]*)\}" line)
        targets (if-let [[_ inside] tgt-match]
                  (if (str/blank? inside)
                    []
                    (mapv #(Integer/parseInt (str/trim %))
                          (str/split inside #",")))
                  [])]
    [buttons targets]))

(defn min-int [a b]
  (if (< a b) a b))

(defn solve [buttons targets]
  (let [n (count targets)
        m (count buttons)
        matrix (make-array Double/TYPE n (inc m))]
    
    ;; Initialize matrix
    (doseq [j (range n)]
      (aset matrix j m (double (targets j))))
    (doseq [i (range m)
            j (buttons i)
            :when (< j n)]
      (aset matrix j i 1.0))
    
    ;; Gaussian elimination
    (let [pivot-col (int-array n -1)]
      (loop [row 0, col 0]
        (when (and (< row n) (< col m))
          (let [max-row (loop [r (inc row) max-r row]
                          (if (< r n)
                            (if (> (Math/abs (aget matrix r col))
                                   (Math/abs (aget matrix max-r col)))
                              (recur (inc r) r)
                              (recur (inc r) max-r))
                            max-r))]
            (if (< (Math/abs (aget matrix max-row col)) 1e-9)
              (recur row (inc col))
              (do
                ;; Swap rows
                (when (not= row max-row)
                  (let [temp (aget matrix row)]
                    (aset matrix row (aget matrix max-row))
                    (aset matrix max-row temp)))
                
                ;; Scale pivot row
                (let [scale (aget matrix row col)]
                  (doseq [c (range col (inc m))]
                    (aset matrix row c (/ (aget matrix row c) scale))))
                
                ;; Eliminate other rows
                (doseq [r (range n)
                        :when (and (not= r row)
                                   (> (Math/abs (aget matrix r col)) 1e-9))]
                  (let [factor (aget matrix r col)]
                    (doseq [c (range col (inc m))]
                      (aset matrix r c (- (aget matrix r c)
                                          (* factor (aget matrix row c)))))))
                
                (aset pivot-col row col)
                (recur (inc row) (inc col)))))))
      
      (let [row (loop [r 0]
                  (if (and (< r n) (>= (aget pivot-col r) 0))
                    (recur (inc r))
                    r))
            rank row
            is-pivot (boolean-array m false)
            pivot-row (int-array m -1)]
        
        ;; Mark pivot columns
        (doseq [r (range rank)
                :when (>= (aget pivot-col r) 0)]
          (aset is-pivot (aget pivot-col r) true)
          (aset pivot-row (aget pivot-col r) r))
        
        ;; Find free variables
        (let [free-vars (vec (remove #(aget is-pivot %) (range m)))
              max-presses (int-array m Integer/MAX_VALUE)]
          
          ;; Calculate max presses for each button
          (doseq [i (range m)]
            (let [limit (reduce (fn [lim j]
                                  (if (< j n)
                                    (min-int lim (targets j))
                                    lim))
                                Integer/MAX_VALUE
                                (buttons i))]
              (aset max-presses i (if (= limit Integer/MAX_VALUE) 0 limit))))
          
          ;; Sort free variables by max presses
          (let [sorted-free-vars (sort-by #(aget max-presses %) free-vars)
                best (atom Integer/MAX_VALUE)
                free-vals (int-array (count sorted-free-vars) 0)]
            
            ;; Helper to compute pivot values
            (letfn [(compute-pivots []
                      (let [res (int-array m 0)]
                        (doseq [i (range (count sorted-free-vars))]
                          (aset res (nth sorted-free-vars i) (aget free-vals i)))
                        (loop [r (dec rank)]
                          (if (>= r 0)
                            (let [col (aget pivot-col r)]
                              (if (>= col 0)
                                (let [v (loop [c (inc col) val (aget matrix r m)]
                                          (if (< c m)
                                            (recur (inc c) (- val (* (aget matrix r c) (aget res c))))
                                            val))
                                      iv (Math/round v)]
                                  (if (or (> (Math/abs (- v iv)) 1e-6)
                                          (< iv 0)
                                          (> iv (aget max-presses col)))
                                    nil
                                    (do
                                      (aset res col iv)
                                      (recur (dec r)))))
                                (recur (dec r))))
                            (vec res)))))]
              
              ;; Enumerate free variables
              (letfn [(enumerate [idx cur-sum]
                        (when (< cur-sum @best)
                          (if (= idx (count sorted-free-vars))
                            (when-let [arr (compute-pivots)]
                              (let [tot (reduce + arr)]
                                (when (< tot @best)
                                  (reset! best tot))))
                            (let [fv (nth sorted-free-vars idx)
                                  limit (aget max-presses fv)]
                              (loop [v 0]
                                (when (<= v limit)
                                  (aset free-vals idx v)
                                  (enumerate (inc idx) (+ cur-sum v))
                                  (recur (inc v))))))))]
                
                (enumerate 0 0)
                (if (= @best Integer/MAX_VALUE) -1 @best)))))))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [lines (->> (line-seq rdr)
                     (remove str/blank?)
                     (map parse-line))
          total (reduce + (map (fn [[btn tgt]] (solve btn tgt)) lines))]
      (println total))))

;; Run the program
(-main)
