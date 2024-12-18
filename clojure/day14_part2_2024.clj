
(require '[clojure.string :as str])

(defn parse-line [line]
  (let [[_ x y vx vy] (re-matches #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)" line)]
    (map #(Integer/parseInt %) [x y vx vy])))

(defn mod [a b]
  (rem (+ (rem a b) b) b))

(defn move-robots [robots size-x size-y]
  (map (fn [[x y vx vy]]
         [(mod (+ x vx) size-x) (mod (+ y vy) size-y) vx vy])
       robots))

(defn count-quadrants [robots size-x size-y]
  (let [center-x (quot size-x 2)
        center-y (quot size-y 2)]
    (reduce (fn [counts [x y _ _]]
              (cond
                (< x center-x) (if (< y center-y) (update counts 0 inc) (if (> y center-y) (update counts 1 inc) counts))
                (> x center-x) (if (< y center-y) (update counts 2 inc) (if (> y center-y) (update counts 3 inc) counts))
                :else counts))
            [0 0 0 0] robots)))

(defn has-no-overlaps? [robots]
  (let [positions (set (map (fn [[x y _ _]] [x y]) robots))]
    (= (count robots) (count positions))))

(defn draw-grid [robots size-x size-y]
  (let [robot-positions (set (map (fn [[x y _ _]] [x y]) robots))]
    (doseq [y (range size-y)]
      (println
       (str/join ""
                 (for [x (range size-x)]
                   (if (contains? robot-positions [x y]) "#" ".")))))))

(defn solve []
  (let [size-x 101
        size-y 103
        robots (->> "input.txt"
                    slurp
                    str/split-lines
                    (remove str/blank?)
                    (map parse-line))]
    (let [robots-part1 (atom robots)]
      (dotimes [_ 100]
        (swap! robots-part1 move-robots size-x size-y))
      (let [counts (count-quadrants @robots-part1 size-x size-y)
            safety-factor (reduce * counts)]
        (println (format "Part 1 - Safety Factor after 100 seconds: %d" safety-factor))))
    (let [robots-part2 (atom robots)]
      (loop [seconds 0]
        (if (has-no-overlaps? @robots-part2)
          (do
            (println (format "Part 2 - Fewest seconds to display Easter egg: %d" seconds))
            (println "Final positions of robots:")
            (draw-grid @robots-part2 size-x size-y))
          (if (> seconds 1000000)
            (println "Exceeded maximum iterations without finding a unique position configuration.")
            (do
              (swap! robots-part2 move-robots size-x size-y)
              (recur (inc seconds)))))))))

(solve)
