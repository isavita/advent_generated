
(ns fuel
  (:require [clojure.string :as str]))

(defn parse-chemical [s]
  (let [[amount name] (str/split s #" ")]
    {:name name :amount (Long/parseLong amount)}))

(defn parse-input [filename]
  (let [lines (str/split-lines (slurp filename))
        reactions (atom {})
        ingredients (atom {})]
    (doseq [line lines
            :let [parts (str/split line #" => ")
                  output (parse-chemical (second parts))
                  inputs (mapv parse-chemical (str/split (first parts) #", "))]]
      (swap! reactions assoc (:name output) output)
      (swap! ingredients assoc (:name output) inputs))
    [@reactions @ingredients]))

(defn calculate-ore [chem amount reactions ingredients surplus]
  (if (= chem "ORE")
    amount
    (let [available (get @surplus chem 0)]
      (if (>= available amount)
        (do (swap! surplus update chem - amount) 0)
        (let [required (- amount available)]
          (swap! surplus assoc chem 0)
          (let [reaction (get reactions chem)
                times (long (Math/ceil (/ required (:amount reaction))))
                ore (atom 0)]
            (doseq [ingredient (get ingredients chem)]
              (swap! ore + (calculate-ore (:name ingredient)
                                           (* (:amount ingredient) times)
                                           reactions ingredients surplus)))
            (swap! surplus assoc chem (- (* times (:amount reaction)) required))
            @ore))))))

(defn -main []
  (let [[reactions ingredients] (parse-input "input.txt")
        surplus (atom {})]
    (println (calculate-ore "FUEL" 1 reactions ingredients surplus))))

(-main)
