(ns passport-processing
  (:require [clojure.string :as str]))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn parse-passport [passport]
  (into {} (map #(str/split % #":") (str/split passport #"\s+"))))

(defn valid-passport? [passport]
  (every? #(contains? passport %) required-fields))

(defn count-valid-passports [input]
  (->> (str/split input #"\n\n")
       (map parse-passport)
       (filter valid-passport?)
       count))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [input (slurp rdr)]
      (println (count-valid-passports input)))))

(-main)