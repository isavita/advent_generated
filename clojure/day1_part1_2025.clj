
(ns dial
  (:require [clojure.string :as str]))

(defn main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [current-pos (atom 50)
          zero-count (atom 0)]
      (doseq [line (line-seq rdr)]
        (when-let [line (str/trim line)]
          (when (not (empty? line))
            (let [direction (first line)
                  amount (Integer/parseInt (subs line 1))]
              (swap! current-pos
                     (fn [pos]
                       (let [new-pos (if (= direction \R)
                                       (mod (+ pos amount) 100)
                                       (let [new-pos (mod (- pos amount) 100)]
                                         (if (neg? new-pos) (+ new-pos 100) new-pos)))]
                         (when (= new-pos 0) (swap! zero-count inc))
                         new-pos)))))))
      (println "The password is:" @zero-count))))

(main)
