(ns day4
  (:require [clojure.string :as str]))

(defn valid-passphrase? [passphrase]
  (= (count (str/split passphrase #"\s+"))
     (count (set (str/split passphrase #"\s+")))))

(defn count-valid-passphrases [input]
  (->> input
       (str/split-lines)
       (filter valid-passphrase?)
       (count)))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (println (count-valid-passphrases (slurp rdr)))))

(-main)