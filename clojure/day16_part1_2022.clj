
(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        valves (atom {})]
    (doseq [line lines]
      (let [[valve-info tunnels-info] (str/split line #"; ")
            id (second (re-find #"Valve (\w+)" valve-info))
            flow (Integer/parseInt (second (re-find #"rate=(\d+)" valve-info)))
            tunnels (->> (str/replace tunnels-info #"tunnel(s)? lead(s)? to valve(s)? " "")
                         (#(str/split % #", "))
                         (map str/trim)
                         vec)]
        (swap! valves assoc id {:flow flow :tunnels (zipmap tunnels (repeat 1)) id 0})))
    @valves))

(defn floyd-warshall [valves]
  (let [ks (keys valves)]
    (reduce
      (fn [dist [k i j]]
        (let [dik (get-in dist [i k] ##Inf)
              dkj (get-in dist [k j] ##Inf)
              dij (get-in dist [i j] ##Inf)]
          (if (< (+ dik dkj) dij)
            (assoc-in dist [i j] (+ dik dkj))
            dist)))
      (reduce
        (fn [dist id]
          (assoc dist id (get-in valves [id :tunnels])))
        {}
        ks)
      (for [k ks i ks j ks] [k i j]))))

(defn max-pressure [valves dist curr minute pressure open]
  (reduce
    (fn [max-val next-valve]
      (let [new-open (disj open next-valve)
            time-left (- minute (get-in dist [curr next-valve]) 1)]
        (if (pos? time-left)
          (max max-val
               (max-pressure valves
                            dist
                            next-valve
                            time-left
                            (+ pressure (* time-left (get-in valves [next-valve :flow])))
                            new-open))
          max-val)))
    pressure
    open))

(defn -main []
  (let [input (slurp "input.txt")
        valves (parse-input input)
        dist (floyd-warshall valves)
        open-valves (set (filter #(pos? (get-in valves [% :flow])) (keys valves)))]
    (println (max-pressure valves dist "AA" 30 0 open-valves))))

(-main)
