(ns day18
  (:gen-class))

(defn read-first-row [filename]
  (with-open [r (clojure.java.io/reader filename)]
    (first (line-seq r))))

(defn is-trap [left center right row]
  (let [l (if (neg? left) \. (nth row left nil))
        c (nth row center \.)
        r (if (>= right (count row)) \. (nth row right \.))]
    (or (and (= \^ l) (= \^ c) (= \. r))
        (and (= \^ c) (= \^ r) (= \. l))
        (and (= \^ l) (= \. c) (= \. r))
        (and (= \^ r) (= \. c) (= \. l)))))

(defn count-safe-tiles [first-row total-rows]
  (let [row (atom first-row)
        safe-count (atom (count (filter #{\.} @row)))]
    (dotimes [_ (dec total-rows)]
      (let [next-row (apply str (for [i (range (count @row))]
                                   (if (is-trap (- i 1) i (+ i 1) @row) \^ \.)))]
        (swap! safe-count + (count (filter #{\.} next-row)))
        (reset! row next-row)))
    @safe-count))

(defn -main []
  (let [first-row (read-first-row "input.txt")]
    (println (count-safe-tiles first-row 400000))))

(-main)