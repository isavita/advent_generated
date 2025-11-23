
(ns hot-springs
  (:require [clojure.string :as str]))

(def memo (atom {}))

(defn parse-line [line]
  (let [[springs groups] (str/split line #" ")]
    [(into [] springs)
     (mapv #(Long/parseLong %) (str/split groups #","))]))

(defn unfold [springs groups times]
  [(str/join "?" (repeat times (apply str springs)))
   (vec (apply concat (repeat times groups)))])

(defn count-arrangements [springs groups]
  (let [slen (count springs)
        glen (count groups)]
    (letfn [(dp [i g cnt]
              (cond
                (= i slen)
                (if (and (= g glen) (zero? cnt)) 1
                    (if (and (= g (dec glen)) (= cnt (nth groups g))) 1 0))

                (some? (get @memo [i g cnt]))
                (get @memo [i g cnt])

                :else
                (let [c (nth springs i)
                      res (atom 0)]
                  (when (or (= c \.) (= c \?))
                    (swap! res +
                           (if (zero? cnt)
                             (dp (inc i) g 0)
                             (if (and (< g glen) (= cnt (nth groups g)))
                               (dp (inc i) (inc g) 0)
                               0))))
                  (when (or (= c \#) (= c \?))
                    (when (and (< g glen) (< cnt (nth groups g)))
                      (swap! res + (dp (inc i) g (inc cnt)))))
                  (let [v @res]
                    (swap! memo assoc [i g cnt] v)
                    v))))]
      (dp 0 0 0))))

(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))]
    (println
     (transduce
      (comp
       (map parse-line)
       (map (fn [[s g]] (unfold s g 5)))
       (map (fn [[s g]] (do (reset! memo {}) (count-arrangements s g)))))
      + 0 lines))))

(-main)
