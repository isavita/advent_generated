
(ns solution
  (:import (java.io BufferedReader FileReader)))

(defn first-n-unique [s n]
  (loop [i n]
    (if (>= i (count s))
      -1
      (let [b (subs s (- i n) i)
            set-of (set b)]
        (if (= (count b) (count set-of))
          i
          (recur (inc i)))))))

(defn read-all [path]
  (with-open [reader (BufferedReader. (FileReader. path))]
    (-> reader .readLine .trim)))

(defn -main []
  (let [s (read-all "input.txt")]
    (println (first-n-unique s 4))))

(-main)
