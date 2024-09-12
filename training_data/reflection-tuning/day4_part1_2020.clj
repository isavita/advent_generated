(ns passport-processing
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn parse-passport [passport-str]
  (->> (str/split passport-str #"\s+")
       (map #(str/split % #":"))
       (into {})))

(defn valid-passport? [passport]
  (set/subset? required-fields (set (keys passport))))

(defn count-valid-passports [input]
  (->> (str/split input #"\n\n")
       (map parse-passport)
       (filter valid-passport?)
       count))

(defn -main []
  (let [input (slurp "input.txt")
        valid-count (count-valid-passports input)]
    (println valid-count)))

(-main)
