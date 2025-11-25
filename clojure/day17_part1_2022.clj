
(ns tetris
  (:require [clojure.string :as str]))

(def rock-str "####

 # 
###
 # 

  #
  #
###

#
#
#
#

##
##")

(def directions {:N [0 1] :E [1 0] :S [0 -1] :W [-1 0]})
(def from-byte {\< :W \> :E})

(defn parse-jet [file]
  (vec (str/trim (slurp file))))

(defn parse-rocks [s]
  (for [rock (str/split s #"\n\n")]
    (vec (for [[y line] (map-indexed vector (reverse (str/split-lines rock)))
               [x ch] (map-indexed vector line)
               :when (= ch \#)]
           [x y]))))

(defn collision? [grid rock [dx dy]]
  (some (fn [[x y]]
          (let [nx (+ x dx) ny (+ y dy)]
            (or (grid [nx ny]) (< nx 0) (> nx 6))))
        rock))

(defn place-rock [grid rock [dx dy]]
  (into grid (for [[x y] rock] [(+ x dx) (+ y dy)])))

(defn drop-rock [grid jet rock j floor]
  (loop [pos [2 (+ floor 4)] j j]
    (let [jet-dir (jet j)
          j (mod (inc j) (count jet))
          [dx dy] (directions (from-byte jet-dir))
          new-pos [(+ (first pos) dx) (+ (second pos) dy)]
          pos (if (collision? grid rock new-pos) pos new-pos)
          down-pos [(first pos) (dec (second pos))]]
      (if (collision? grid rock down-pos)
        [(place-rock grid rock pos) j (apply max (cons floor (map #(+ (second %) (second pos)) rock)))]
        (recur down-pos j)))))

(defn simulate [jet rocks rounds]
  (loop [grid (set (for [x (range 7)] [x 0]))
         j 0
         floor 0
         i 0
         curr 0]
    (if (= i rounds)
      floor
      (let [[grid j floor] (drop-rock grid jet (nth rocks curr) j floor)]
        (recur grid j floor (inc i) (mod (inc curr) (count rocks)))))))

(defn -main []
  (let [jet (parse-jet "input.txt")
        rocks (parse-rocks rock-str)]
    (println (simulate jet rocks 2022))))

(-main)
