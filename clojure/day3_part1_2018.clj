(ns day3
  (:gen-class))

(defn parse-claim [line]
  (let [[_ id x y w h] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)]
    {:id (Integer/parseInt id) :x (Integer/parseInt x) :y (Integer/parseInt y) :w (Integer/parseInt w) :h (Integer/parseInt h)}))

(defn claim-to-points [claim]
  (for [x (range (:x claim) (+ (:x claim) (:w claim)))
        y (range (:y claim) (+ (:y claim) (:h claim)))]
    [x y]))

(defn count-overlaps [claims]
  (let [points (mapcat claim-to-points claims)
        freqs (frequencies points)]
    (count (filter #(> % 1) (vals freqs)))))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [claims (map parse-claim (line-seq rdr))]
      (println "Overlapping square inches:" (count-overlaps claims)))))

(-main)