(require '[clojure.string :as str])

(defn abba? [s]
  (and (= 4 (count s))
       (= (first s) (nth s 3))
       (= (second s) (nth s 2))
       (not= (first s) (second s))))

(defn contains-abba? [s]
  (some abba? (partition 4 1 s)))

(defn supports-tls? [ip]
  (let [parts (str/split ip #"[\[\]]")
        outside-brackets (take-nth 2 parts)
        inside-brackets (take-nth 2 (rest parts))]
    (and (some contains-abba? outside-brackets)
         (not-any? contains-abba? inside-brackets))))

(defn count-tls-support [input]
  (->> (str/split-lines input)
       (filter supports-tls?)
       count))

;; Example usage:
;; (def input (slurp "path/to/input/file.txt"))
;; (println (count-tls-support input))
