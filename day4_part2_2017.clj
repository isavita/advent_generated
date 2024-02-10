
(defn valid-passphrase [passphrase]
  (let [words (clojure.string/split passphrase #" ")
        sorted-words (map #(apply str (sort %)) words)]
    (= (count words) (count (distinct words)))))

(defn valid-passphrase-part-two [passphrase]
  (let [words (clojure.string/split passphrase #" ")
        sorted-words (map #(apply str (sort %)) words)]
    (= (count words) (count (distinct sorted-words)))))

(defn count-valid-passphrases [passphrases]
  (count (filter valid-passphrase passphrases)))

(defn count-valid-passphrases-part-two [passphrases]
  (count (filter valid-passphrase-part-two passphrases)))

(def input (slurp "input.txt"))
(def passphrases (clojure.string/split-lines input))

(println (count-valid-passphrases passphrases))
(println (count-valid-passphrases-part-two passphrases))
