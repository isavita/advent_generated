(ns solution
  (:require [clojure.string :as str]))

(defn parse-claim [line]
  (let [[_ id x y w h] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)]
    {:id (Integer/parseInt id) :x (Integer/parseInt x) :y (Integer/parseInt y) :w (Integer/parseInt w) :h (Integer/parseInt h)}))

(defn main []
  (let [claims (map parse-claim (str/split-lines (slurp "input.txt")))
        fabric (atom {})
        overlaps (atom {})]
    (doseq [claim claims]
      (doseq [x (range (:x claim) (+ (:x claim) (:w claim)))
              y (range (:y claim) (+ (:y claim) (:h claim)))]
        (let [key [x y]]
          (swap! fabric update key (fnil inc 0))
          (when (> (get @fabric key) 1)
            (swap! overlaps assoc key true)))))
    (doseq [claim claims]
      (let [overlap (atom false)]
        (doseq [x (range (:x claim) (+ (:x claim) (:w claim)))
                y (range (:y claim) (+ (:y claim) (:h claim)))]
          (when (get @overlaps [x y])
            (reset! overlap true)))
        (when-not @overlap
          (println (:id claim))
          (System/exit 0))))))

(main)