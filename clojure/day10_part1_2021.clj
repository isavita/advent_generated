
(defn solve []
  (let [pairs {\) \( \] \[ \} \{ \> \<}
        scores {\) 3 \] 57 \} 1197 \> 25137}]
    (with-open [rdr (clojure.java.io/reader "input.txt")]
      (->> (line-seq rdr)
           (reduce (fn [total line]
                     (loop [stack [] chars (seq line)]
                       (if (empty? chars)
                         total
                         (let [char (first chars)]
                           (case char
                             (\( \[ \{ \<) (recur (conj stack char) (rest chars))
                             (\) \] \} \>) (if (or (empty? stack) (not= (peek stack) (pairs char)))
                                             (+ total (scores char))
                                             (recur (pop stack) (rest chars)))
                             (recur stack (rest chars)))))))
                   0)))))

(println (solve))
