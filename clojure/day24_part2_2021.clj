
(ns submarine
  (:require [clojure.string :as str]))

(defn get-nth-token [line n]
  (nth (str/split line #"\s+") n))

(defn num [digits]
  (reduce (fn [acc d] (+ (* acc 10) d)) 0 digits))

(defn -main [& _]
  (let [lines (str/split-lines (slurp "input.txt"))
        k (atom [])
        l (atom [])
        m (atom [])
        _ (doseq [[i line] (map-indexed vector lines)]
            (case (mod i 18)
              4  (swap! l conj (Integer/parseInt (get-nth-token line 2)))
              5  (swap! k conj (Integer/parseInt (get-nth-token line 2)))
              15 (swap! m conj (Integer/parseInt (get-nth-token line 2)))
              nil))
        constraints (atom {})
        stack (atom [])]
    (doseq [j (range (count @l))]
      (case (@l j)
        1 (swap! stack conj j)
        26 (let [pop-idx (peek @stack)]
             (swap! stack pop)
             (swap! constraints assoc pop-idx [j (+ (@m pop-idx) (@k j))]))))
    (let [min-val (vec (repeat 14 0))
          min-val (reduce (fn [acc idx]
                            (if-let [[target delta] (@constraints idx)]
                              (let [digit-i (loop [d 1] (if (>= (+ d delta) 1) d (recur (inc d))))]
                                (-> acc
                                    (assoc idx digit-i)
                                    (assoc target (+ digit-i delta))))
                              acc))
                          min-val (range 14))]
      (println (num min-val)))))

(-main)
