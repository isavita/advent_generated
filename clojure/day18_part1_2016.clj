
(defn read-first-row [filename]
  (first (line-seq (clojure.java.io/reader filename))))

(defn is-trap [left center right row]
  (let [l (if (or (< left 0) (>= left (count row))) \. (nth row left))
        c (nth row center)
        r (if (or (< right 0) (>= right (count row))) \. (nth row right))]
    (or (and (= l \^) (= c \^) (= r \.))
        (and (= c \^) (= r \^) (= l \.))
        (and (= l \^) (= c \.) (= r \.))
        (and (= r \^) (= c \.) (= l \.)))))

(defn next-row [row]
  (apply str (map-indexed (fn [idx _] (if (is-trap (dec idx) idx (inc idx) row) \^ \.)) row)))

(defn solve [filename total-rows]
  (let [first-row (read-first-row filename)]
    (loop [row first-row
           safe-count (count (filter #(= % \.) row))
           rows-left (dec total-rows)]
      (if (zero? rows-left)
        safe-count
        (let [next-r (next-row row)
              safe-c (count (filter #(= % \.) next-r))]
          (recur next-r (+ safe-count safe-c) (dec rows-left)))))))

(println (solve "input.txt" 40))
