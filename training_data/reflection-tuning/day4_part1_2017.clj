(require '[clojure.string :as str])

(defn valid-passphrase? [phrase]
  (let [words (str/split phrase #"\s+")]
    (= (count words) (count (set words)))))

(defn count-valid-passphrases [input]
  (->> input
       str/split-lines
       (reduce (fn [acc phrase]
                 (if (valid-passphrase? phrase)
                   (inc acc)
                   acc))
               0)))

(def result (count-valid-passphrases (slurp "input.txt")))
(println result)
