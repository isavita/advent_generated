(defn valid-passphrase? [passphrase]
  (let [words (clojure.string/split passphrase #"\s+")]
    (= (count words) (count (set words)))))

(defn count-valid-passphrases [input]
  (->> (clojure.string/split-lines input)
       (filter valid-passphrase?)
       count))

(defn solve [input]
  (count-valid-passphrases input))
