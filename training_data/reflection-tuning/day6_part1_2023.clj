(require '[clojure.string :as str])

(defn parse-input [file]
  (let [lines (str/split-lines (slurp file))
        parse-line #(map read-string (re-seq #"\d+" %))]
    (map vector (parse-line (second lines)) (parse-line (first lines)))))

(defn count-winning-strategies [record-distance race-time]
  (count
   (filter #(> % record-distance)
           (for [hold-time (range (inc race-time))]
             (* hold-time (- race-time hold-time))))))

(defn solve [races]
  (reduce * (map #(apply count-winning-strategies %) races)))

(defn -main []
  (let [races (parse-input "input.txt")
        result (solve races)]
    (println result)))

(-main)
