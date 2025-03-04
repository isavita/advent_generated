
(ns blizzard-basin
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        blizzards (for [y (range height)
                        x (range width)
                        :let [c (get-in lines [y x])]
                        :when (contains? #{\> \< \^ \v} c)]
                    {:pos [x y] :dir c})]
    {:height height :width width :blizzards blizzards}))

(defn move-blizzard [blizzard width height]
  (let [[x y] (:pos blizzard)
        dir (:dir blizzard)]
    (case dir
      \> {:pos [(mod (inc x) width) y] :dir \>}
      \< {:pos [(mod (dec x) width) y] :dir \<}
      \^ {:pos [x (mod (dec y) height)] :dir \^}
      \v {:pos [x (mod (inc y) height)] :dir \v})))

(defn blizzard-positions [blizzards]
  (set (map :pos blizzards)))

(defn wrap-coords [x y width height]
   [(if (zero? x) (- width 2) (if (= x (- width 1)) 1 x))
   (if (zero? y) (- height 2) (if (= y (- height 1)) 1 y))])

(defn move-blizzards [blizzards width height]
    (map
    #(let [new-bliz (move-blizzard % width height)]
         (let [[new-x new-y] (:pos new-bliz)]
            {:pos (wrap-coords new-x new-y width height) :dir (:dir new-bliz)}
         ))
        blizzards)
)

(defn solve [input]
  (let [{:keys [height width blizzards]} (parse-input input)
        start [1 0]
        end [(- width 2) (- height 1)]
        initial-state {:pos start :time 0 :blizzards blizzards}]

    (loop [q (conj clojure.lang.PersistentQueue/EMPTY initial-state)
           visited #{[start 0]}]
      (let [current (peek q)
            q (pop q)]

          (if (= (:pos current) end)
            (:time current)

            (let [next-time (inc (:time current))
                  next-blizzards (move-blizzards (:blizzards current) width height)
                  next-blizzard-pos (blizzard-positions next-blizzards)
                  [x y] (:pos current)
                  moves [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y] [x y]]
                  valid-moves (filter (fn [[nx ny]]
                                          (and (>= nx 0) (< nx width)
                                               (>= ny 0) (< ny height)
                                               (not (contains? #{\#} (get-in (str/split-lines input) [ny nx])))
                                               (not (contains? next-blizzard-pos [nx ny]))))
                                        moves)
                  next-states (for [move valid-moves
                                    :let [next-state {:pos move :time next-time :blizzards next-blizzards}]
                                    :when (not (contains? visited [move next-time]))]
                                next-state)]

              (recur (reduce conj q next-states)
                     (into visited (map (fn [s] [(:pos s) (:time s)]) next-states)))))))))

(defn -main []
  (let [input (slurp "input.txt")]
    (println (solve input))))

(-main)
