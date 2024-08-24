(ns spinlock
  (:require [clojure.java.io :as io]))

(defn spinlock [steps]
  (let [buffer (atom [0])
        current-pos (atom 0)]
    (doseq [i (range 1 2018)]
      (swap! current-pos #(mod (+ % steps) (count @buffer)))
      (swap! buffer #(vec (concat (subvec % 0 (inc @current-pos)) [i] (subvec % (inc @current-pos)))))
      (reset! current-pos (inc @current-pos)))
    (nth @buffer (mod (inc @current-pos) (count @buffer)))))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [steps (Integer/parseInt (clojure.string/trim (first (line-seq r))))]
      (println (spinlock steps)))))

(-main)