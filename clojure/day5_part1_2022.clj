
(require '[clojure.string :as str])

(defn parse-stacks [lines]
  (let [num-stacks (quot (+ (count (first lines)) 1) 4)]
    (reduce (fn [stacks line]
              (reduce (fn [stacks i]
                        (let [pos (+ 1 (* 4 i))]
                          (if (< pos (count line))
                            (let [c (nth line pos)]
                              (if (and (char? c) (<= (int \A) (int c) (int \Z)))
                                (update stacks i conj c)
                                stacks))
                            stacks)))
                      stacks
                      (range num-stacks)))
            (vec (repeat num-stacks '()))
            (reverse lines))))

(defn parse-step [step]
  (let [parts (str/split step #"\s+")]
    [(Integer/parseInt (nth parts 1))
     (dec (Integer/parseInt (nth parts 3)))
     (dec (Integer/parseInt (nth parts 5)))]))

(defn move [stacks steps]
  (reduce (fn [stacks step]
            (let [[n from to] (parse-step step)]
              (loop [stacks stacks n n]
                (if (zero? n)
                  stacks
                  (let [crate (peek (stacks from))]
                    (recur (-> stacks
                              (update from pop)
                              (update to conj crate))
                          (dec n)))))))
          stacks
          steps))

(defn solve []
  (let [input (slurp "input.txt")
        [stacks-str steps-str] (str/split input #"\n\n")
        stacks-lines (str/split-lines stacks-str)
        steps-lines (remove str/blank? (str/split-lines steps-str))
        stacks (parse-stacks stacks-lines)
        moved-stacks (move stacks steps-lines)]
    (println (apply str (map peek moved-stacks)))))

(solve)
