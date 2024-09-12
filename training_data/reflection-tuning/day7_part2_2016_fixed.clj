(ns day7
  (:require [clojure.string :as str]))

(defn abba? [s]
  (and (= (count s) 4)
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

(defn aba? [s]
  (and (= (count s) 3)
       (= (first s) (nth s 2))
       (not= (first s) (second s))))

(defn find-abas [s]
  (filter aba? (partition 3 1 s)))

(defn aba-to-bab [aba]
  (str (second aba) (first aba) (second aba)))

(defn supports-ssl? [ip]
  (let [parts (str/split ip #"[\[\]]")
        outside-brackets (apply str (take-nth 2 parts))
        inside-brackets (apply str (take-nth 2 (rest parts)))
        abas (find-abas outside-brackets)
        babs (map aba-to-bab abas)]
    (some #(str/includes? inside-brackets %) babs)))

(defn solve [input]
  (let [ips (str/split-lines input)]
    {:part1 (count (filter supports-tls? ips))
     :part2 (count (filter supports-ssl? ips))}))

(def input (slurp "input.txt"))
(prn (solve input))
