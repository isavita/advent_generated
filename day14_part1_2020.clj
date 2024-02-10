
(ns solution
  (:require [clojure.string :as str]))

(defn apply-mask [value mask]
  (let [mask-len (count mask)]
    (reduce
      (fn [result i]
        (let [bit-value (bit-shift-left 1 (- 35 i))]
          (cond
            (= (get mask i) \1) (bit-or result bit-value)
            (= (get mask i) \X) (bit-or result (bit-and value bit-value))
            :else result)))
      0
      (range mask-len))))

(defn -main []
  (with-open [file (clojure.java.io/reader "input.txt")]
    (let [lines (line-seq file)
          mask (atom "")
          mem (atom {})]
      (doseq [line lines]
        (if (str/starts-with? line "mask = ")
          (reset! mask (subs line 7))
          (let [[_ address value] (re-find #"mem\[(\d+)] = (\d+)" line)]
            (swap! mem assoc (Long/parseLong address) (apply-mask (Long/parseLong value) @mask)))))
      (println (apply + (vals @mem))))))

(-main)
