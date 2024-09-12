(ns passport-processing
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn parse-passports [input]
  (->> (str/split input #"\n\n")
       (map #(into {} (map (fn [kv] (str/split kv #":")) (str/split % #"\s+"))))))

(defn valid-year? [value min max]
  (when-let [year (parse-long value)]
    (<= min year max)))

(defn valid-height? [value]
  (when-let [[_ height unit] (re-matches #"(\d+)(cm|in)" value)]
    (case unit
      "cm" (<= 150 (parse-long height) 193)
      "in" (<= 59 (parse-long height) 76)
      false)))

(defn valid-hair-color? [value]
  (re-matches #"#[0-9a-f]{6}" value))

(defn valid-eye-color? [value]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} value))

(defn valid-passport-id? [value]
  (re-matches #"\d{9}" value))

(defn valid-passport? [passport]
  (and (every? passport required-fields)
       (valid-year? (passport "byr") 1920 2002)
       (valid-year? (passport "iyr") 2010 2020)
       (valid-year? (passport "eyr") 2020 2030)
       (valid-height? (passport "hgt"))
       (valid-hair-color? (passport "hcl"))
       (valid-eye-color? (passport "ecl"))
       (valid-passport-id? (passport "pid"))))

(defn count-valid-passports [passports]
  (count (filter valid-passport? passports)))

(defn solve []
  (with-open [rdr (io/reader "input.txt")]
    (let [input (slurp rdr)
          passports (parse-passports input)]
      (count-valid-passports passports))))

(println (solve))
