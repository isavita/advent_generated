
(ns day17
  (:require [clojure.string :as str])
  (:import (java.security MessageDigest)))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn open-doors [passcode path]
  (let [hash (md5 (str passcode path))
        doors (take 4 hash)]
    (map #(contains? #{\b \c \d \e \f} %) doors)))

(defn solve [passcode]
  (let [target [3 3]]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY {:pos [0 0] :path ""})
           visited #{} ]
      (if (empty? queue)
        nil
        (let [{:keys [pos path]} (peek queue)
              rest-queue (pop queue)]
          (if (= pos target)
            path
            (let [[x y] pos
                  [up down left right] (open-doors passcode path)
                  next-moves (cond-> []
                               up    (conj {:pos [(dec x) y] :path (str path "U")})
                               down  (conj {:pos [(inc x) y] :path (str path "D")})
                               left  (conj {:pos [x (dec y)] :path (str path "L")})
                               right (conj {:pos [x (inc y)] :path (str path "R")}))
                  valid-moves (filter #(and (>= (first (:pos %)) 0)
                                            (<  (first (:pos %)) 4)
                                            (>= (second (:pos %)) 0)
                                            (<  (second (:pos %)) 4)
                                            (not (contains? visited (:path %))))
                                      next-moves)
                  new-queue (reduce conj rest-queue valid-moves)
                  new-visited (reduce conj visited (map :path valid-moves))]

              (recur new-queue new-visited))))))))


(defn -main []
  (let [passcode (str/trim (slurp "input.txt"))]
    (println (solve passcode))))

(-main)
