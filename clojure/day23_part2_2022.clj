
(ns unstable-diffusion
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map-indexed (fn [y row]
                      (->> (seq row)
                           (map-indexed (fn [x c]
                                          (when (= c \#) [x y])))
                           (remove nil?))))
       (apply concat)
       (set)))

(defn neighbors [[x y]]
  [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
   [x (dec y)] [x (inc y)]
   [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]])

(defn occupied? [elves pos]
  (contains? elves pos))

(defn propose-move [elves directions elf]
  (if (every #(not (occupied? elves %)) (neighbors elf))
    elf
    (loop [dirs directions]
      (when (seq dirs)
        (let [dir (first dirs)
              [dx dy] (case dir
                        :n [0 -1]
                        :s [0 1]
                        :w [-1 0]
                        :e [1 0])
              [x y] elf
              proposed-pos [(+ x dx) (+ y dy)]
              check-positions (case dir
                                :n [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]
                                :s [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]
                                :w [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]]
                                :e [[(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]])]
          (if (every #(not (occupied? elves %)) check-positions)
            proposed-pos
            (recur (rest dirs))))))))

(defn move-elves [elves proposed-moves]
  (let [counts (frequencies proposed-moves)
        new-elves (reduce (fn [acc [elf proposed]]
                           (if (not= elf proposed)
                             (if (= 1 (get counts proposed 0))
                               (conj acc proposed)
                               acc)
                             (conj acc elf)))
                         #{}
                         (map vector elves proposed-moves))]
    new-elves))

(defn simulate-round [elves directions]
  (let [proposed-moves (map (partial propose-move elves directions) elves)
        new-elves (move-elves elves proposed-moves)
        new-directions (concat (rest directions) [(first directions)])]
    [new-elves new-directions]))

(defn bounding-box [elves]
  (let [xs (map first elves)
        ys (map second elves)]
    [(apply min xs) (apply min ys) (apply max xs) (apply max ys)]))

(defn count-empty-tiles [elves]
  (let [[min-x min-y max-x max-y] (bounding-box elves)
        width (inc (- max-x min-x))
        height (inc (- max-y min-y))
        area (* width height)]
    (- area (count elves))))

(defn solve-part-1 [elves]
  (loop [round 0
         current-elves elves
         directions [:n :s :w :e]]
    (if (= round 10)
      (count-empty-tiles current-elves)
      (let [[new-elves new-directions] (simulate-round current-elves directions)]
        (recur (inc round) new-elves new-directions)))))

(defn solve-part-2 [elves]
  (loop [round 1
         current-elves elves
         directions [:n :s :w :e]]
    (let [[new-elves new-directions] (simulate-round current-elves directions)]
      (if (= current-elves new-elves)
        round
        (recur (inc round) new-elves new-directions)))))

(defn -main []
  (let [input (slurp "input.txt")
        initial-elves (parse-input input)]
    (println "Part 1:" (solve-part-1 initial-elves))
    (println "Part 2:" (solve-part-2 initial-elves))))

(-main)
