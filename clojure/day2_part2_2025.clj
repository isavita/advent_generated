
(require '[clojure.string :as str])

(defn invalid? [x]
  (let [s (str x)
        n (count s)]
    (when (> n 1)
      (some (fn [p]
              (and (zero? (rem n p))
                   (>= (quot n p) 2)
                   (every? (fn [i]
                             (= (nth s i) (nth s (mod i p))))
                           (range p n))))
            (range 1 (inc (quot n 2)))))))

(defn parse-input [input]
  (->> (str/split input #"[\s,]+")
       (mapcat (fn [token]
                 (when-let [[_ a b] (re-matches #"(\d+)-(\d+)" token)]
                   (let [a (Long/parseLong a)
                         b (Long/parseLong b)]
                     (range (min a b) (inc (max a b)))))))
       (filter invalid?)
       (reduce +)))

(let [input (slurp "input.txt")]
  (println (parse-input input)))
