(defn valid-triangle? [[a b c]]
  (and (> (+ a b) c)
       (> (+ b c) a)
       (> (+ c a) b)))

(defn parse-line [line]
  (map #(Integer/parseInt %) (re-seq #"\d+" line)))

(defn count-valid-triangles [filename]
  (->> (slurp filename)
       clojure.string/split-lines
       (map parse-line)
       (filter valid-triangle?)
       count))

(println (count-valid-triangles "input.txt"))
