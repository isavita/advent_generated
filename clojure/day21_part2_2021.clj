
(ns dirac-dice
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        p1-start (-> lines first (str/split #": ") second Integer/parseInt)
        p2-start (-> lines second (str/split #": ") second Integer/parseInt)]
    [p1-start p2-start]))

(defn move [position steps]
  (inc (mod (+ (dec position) steps) 10)))

(defn play-deterministic-game [p1-start p2-start]
  (loop [p1-pos p1-start
         p2-pos p2-start
         p1-score 0
         p2-score 0
         die-roll 1
         rolls 0
         turn :p1]
    (if (>= p1-score 1000)
      (* p2-score rolls)
      (if (>= p2-score 1000)
        (* p1-score rolls)
        (let [roll-sum (->> (range die-roll (+ die-roll 3))
                            (map #(mod (dec %) 100))
                            (map inc)
                            (reduce +))
              new-rolls (+ rolls 3)
              new-die-roll (mod (+ die-roll 3) 100)
              new-die-roll (if (zero? new-die-roll) 100 new-die-roll)] ; Corrected to 100, not 1.
          (if (= turn :p1)
            (let [new-pos (move p1-pos roll-sum)
                  new-score (+ p1-score new-pos)]
              (recur new-pos p2-pos new-score p2-score new-die-roll new-rolls :p2))
            (let [new-pos (move p2-pos roll-sum)
                  new-score (+ p2-score new-pos)]
              (recur p1-pos new-pos p1-score new-score new-die-roll new-rolls :p1))))))))


(def dirac-rolls
  (for [r1 [1 2 3]
        r2 [1 2 3]
        r3 [1 2 3]]
    (+ r1 r2 r3)))

(def dirac-roll-freqs
  (frequencies dirac-rolls))

(defn play-dirac-game [p1-start p2-start]
  (let [wins (atom [0 0])] ; Use an atom to store wins
    (loop [games {[p1-start p2-start 0 0 :p1] 1}]
      (when (seq games)  ; Check if there are any games left.
        (let [next-games (atom {})]
          (doseq [[[p1-pos p2-pos p1-score p2-score turn] count] games]
            (doseq [[roll-sum freq] dirac-roll-freqs]
              (if (= turn :p1)
                (let [new-pos (move p1-pos roll-sum)
                      new-score (+ p1-score new-pos)
                      new-count (* count freq)]
                  (if (>= new-score 21)
                    (swap! wins update 0 + new-count) ; Update wins directly
                    (swap! next-games update [new-pos p2-pos new-score p2-score :p2] (fnil + 0) new-count)))
                (let [new-pos (move p2-pos roll-sum)
                      new-score (+ p2-score new-pos)
                      new-count (* count freq)]
                  (if (>= new-score 21)
                    (swap! wins update 1 + new-count)
                    (swap! next-games update [p1-pos new-pos p1-score new-score :p1] (fnil + 0) new-count))))))
          (recur @next-games))))
    (max (first @wins) (second @wins))))

(defn -main []
  (let [input (slurp "input.txt")
        [p1-start p2-start] (parse-input input)]
    (println "Part 1:" (play-deterministic-game p1-start p2-start))
    (println "Part 2:" (play-dirac-game p1-start p2-start))))

(-main)
