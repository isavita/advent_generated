(ns allergen-assessment
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[ingredients allergens] (str/split line #"\(")
        ingredients (str/split (str/trim ingredients) #"\s+")
        allergens (str/split (str/trim (subs allergens 9 (dec (count allergens)))) #",\s+")]
    [ingredients allergens]))

(defn read-input []
  (with-open [r (io/reader "input.txt")]
    (reduce (fn [acc line]
              (let [[ingredients allergens] (parse-line line)]
                (reduce (fn [acc allergen]
                          (update acc allergen (fn [v] (if v (conj v ingredients) [ingredients]))))
                        acc
                        allergens)))
            {}
            (line-seq r))))

(defn intersect-all [lists]
  (apply set/intersection (map set lists)))

(defn find-allergen-ingredient [allergen-map]
  (loop [allergen-map allergen-map
         resolved {}]
    (if (empty? allergen-map)
      resolved
      (let [resolved-ingredients (set (vals resolved))
            unresolved (reduce (fn [acc [allergen lists]]
                                 (let [possible (set/difference (intersect-all lists) resolved-ingredients)]
                                   (if (= 1 (count possible))
                                     (assoc acc allergen (first possible))
                                     acc)))
                               {}
                               allergen-map)]
        (recur (reduce (fn [acc [allergen lists]]
                         (let [possible (set/difference (intersect-all lists) resolved-ingredients)]
                           (if (= 1 (count possible))
                             (dissoc acc allergen)
                             (assoc acc allergen lists))))
                       {}
                       allergen-map)
               (merge resolved unresolved))))))

(defn main []
  (let [allergen-map (read-input)
        allergen-ingredient (find-allergen-ingredient allergen-map)]
    (->> allergen-ingredient
         (sort-by key)
         (map val)
         (str/join ",")
         println)))

(main)