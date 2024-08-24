(ns solution
  (:require [clojure.string :as str]))

(defn parse-room [line]
  (let [parts (re-matches #"([a-z-]+)-(\d+)\[([a-z]+)\]" line)]
    (when parts
      {:name (nth parts 1)
       :sector-id (Integer/parseInt (nth parts 2))
       :checksum (nth parts 3)})))

(defn calculate-checksum [name]
  (let [letters (apply str (re-seq #"[a-z]" name))
        freqs (frequencies letters)]
    (->> freqs
         (sort-by (juxt (comp - val) key))
         (take 5)
         (map key)
         (apply str))))

(defn is-real-room [room]
  (= (calculate-checksum (:name room)) (:checksum room)))

(defn decrypt-name [name sector-id]
  (->> name
       (map #(if (= % \-) \space (char (+ (mod (+ (- (int %) 97) sector-id) 26) 97))))
       (apply str)))

(defn find-north-pole-room [rooms]
  (some #(when (and (is-real-room %) (str/includes? (decrypt-name (:name %) (:sector-id %)) "northpole")) (:sector-id %))
        rooms))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [rooms (map parse-room (line-seq rdr))]
      (println (find-north-pole-room rooms)))))

(-main)