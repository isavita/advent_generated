
(ns stoneblink
  (:gen-class))

(defn trim-leading-zeros [s]
  (loop [i 0]
    (cond
      (= i (dec (count s))) (str (nth s i))
      (= (nth s i) \0) (recur (inc i))
      :else (subs s i))))

(defn split-stone [s]
  (let [mid (quot (count s) 2)
        left  (trim-leading-zeros (subs s 0 mid))
        right (trim-leading-zeros (subs s mid))]
    [(if (empty? left) "0" left)
     (if (empty? right) "0" right)]))

(defn multiply-by-2024 [s]
  (let [n (bigint s)]
    (str (* n 2024N))))

(defn step-stones [stones]
  (reduce-kv
    (fn [acc stone cnt]
      (cond
        (= stone "0") (update acc "1" (fnil + 0) cnt)
        (even? (count stone))
        (let [[l r] (split-stone stone)]
          (-> acc
              (update l (fnil + 0) cnt)
              (update r (fnil + 0) cnt)))
        :else
        (let [ns (multiply-by-2024 stone)]
          (update acc ns (fnil + 0) cnt))))
    {} stones))

(defn -main []
  (let [line (first (line-seq (clojure.java.io/reader "input.txt")))
        initial (frequencies (clojure.string/split line #"\s+"))
        final (reduce (fn [stones _] (step-stones stones)) initial (range 75))]
    (println (reduce + (vals final)))))

(-main)
