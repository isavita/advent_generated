
(defn score [input]
  (let [cleaned-input (clojure.string/replace input #"!.{1}" "")
        cleaned-input-no-garbage (clojure.string/replace cleaned-input #"<[^>]*>" "")
        cleaned-input-no-comma (clojure.string/replace cleaned-input-no-garbage #"," "")
        depth (reduce (fn [[score depth] char]
                        (cond
                          (= char \{) [(+ score depth 1) (inc depth)]
                          (= char \}) [(dec score) (dec depth)]
                          :else [score depth]))
                      [0 1]
                      cleaned-input-no-comma)]
    (first depth)))

(def input (slurp "input.txt"))
(println (score input))
