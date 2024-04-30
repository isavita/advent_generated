(ns passport-processing
  (:require [clojure.string :as str]))

(defn parse-passport [passport]
  (let [fields (str/split passport #"\s+")]
    (reduce (fn [acc field]
              (let [[k v] (str/split field #":")]
                (assoc acc (keyword k) v)))
            {}
            fields)))

(defn valid-birth-year [byr]
  (and (re-matches #"\d{4}" byr)
       (<= 1920 (Integer/parseInt byr) 2002)))

(defn valid-issue-year [iyr]
  (and (re-matches #"\d{4}" iyr)
       (<= 2010 (Integer/parseInt iyr) 2020)))

(defn valid-expiration-year [eyr]
  (and (re-matches #"\d{4}" eyr)
       (<= 2020 (Integer/parseInt eyr) 2030)))

(defn valid-height [hgt]
  (if (str/ends-with? hgt "cm")
    (and (re-matches #"\d+cm" hgt)
         (<= 150 (Integer/parseInt (str/replace hgt "cm" "")) 193))
    (and (re-matches #"\d+in" hgt)
         (<= 59 (Integer/parseInt (str/replace hgt "in" "")) 76))))

(defn valid-hair-color [hcl]
  (re-matches #"#[0-9a-f]{6}" hcl))

(defn valid-eye-color [ecl]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl))

(defn valid-passport-id [pid]
  (and (re-matches #"\d{9}" pid)
       (= 9 (count pid))))

(defn valid-passport [passport]
  (let [{:keys [byr iyr eyr hgt hcl ecl pid]} passport]
    (and byr iyr eyr hgt hcl ecl pid
         (valid-birth-year byr)
         (valid-issue-year iyr)
         (valid-expiration-year eyr)
         (valid-height hgt)
         (valid-hair-color hcl)
         (valid-eye-color ecl)
         (valid-passport-id pid))))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [passports (str/split (slurp rdr) #"\n\n")]
      (println (count (filter valid-passport (map parse-passport passports)))))))

(-main)