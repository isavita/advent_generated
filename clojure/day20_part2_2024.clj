
;; Day 20: Race Condition
;; Reads input.txt and prints number of cheats that save at least 100 picoseconds.
;; No external libraries.

(ns aoc.day20
  (:require [clojure.string :as str]))

(defn read-grid []
  (let [lines (-> (slurp "input.txt") str/split-lines vec)
        h (count lines)
        w (count (first lines))
        grid (into {} (for [y (range h)
                             x (range w)]
                         [[x y] (nth (nth lines y) x)]))]
    {:grid grid :w w :h h}))

(defn find-ch [grid target]
  (->> grid
       (filter (fn [[_ v]] (= v target)))
       ffirst))

(defn in-bounds? [{:keys [w h]} [x y]]
  (and (<= 0 x) (< x w) (<= 0 y) (< y h)))

(defn neigh4 [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn passable? [grid p]
  (let [v (get grid p \#)]
    (not= v \#)))

(defn build-distance-map [{:keys [grid] :as _} start]
  ;; For this puzzle, the path from S to E is unique.
  ;; We'll compute distances along that path by walking.
  (let [track-neigh (->> (neigh4 start) (filter #(passable? grid %)) vec)
        _ (assert (= 1 (count track-neigh)) "Start must have exactly one track neighbor on unique path.")
        first-step (first track-neigh)]
    (loop [prev start
           cur first-step
           dist {start 0
                  cur 1}]
      (let [opts (->> (neigh4 cur) (filter #(passable? grid %)) vec)
            ;; next is the passable neighbor that isn't prev (unique path)
            nxt (first (remove #(= % prev) opts))]
        (if (nil? nxt)
          dist
          (recur cur nxt (assoc dist nxt (inc (dist cur)))))))))

(defn solve []
  (let [{:keys [grid] :as m} (read-grid)
        s (find-ch grid \S)
        e (find-ch grid \E)
        dist (build-distance-map m s)
        total (dist e)]
    (let [threshold 100
          max-cheat 20
          ;; Precompute all offsets (dx,dy) with manhattan <= 20.
          offsets (vec (for [dx (range (- max-cheat) (inc max-cheat))
                              dy (range (- max-cheat) (inc max-cheat))
                              :when (<= (+ (Math/abs dx) (Math/abs dy)) max-cheat)]
                          [dx dy]))]
      ;; For each cheat: it starts at position a on normal track, ends at b on normal track,
      ;; and requires there exists a path of length t (<=20) from a to b that can go through walls.
      ;; Because we can ignore walls during cheat, the minimal time equals Manhattan distance.
      ;; Therefore, cheat duration can be any t with manhattan(a,b) <= t <= 20, and saving depends on
      ;; using exactly t where new time = dist[a] + t + (total - dist[b]).
      ;;
      ;; Equivalent: saved = (total - dist[s])? -> simplify:
      ;; Standard time = total.
      ;; Cheating time using duration t from a to b: dist[a] + t + (total - dist[b]).
      ;; Saving = total - (dist[a] + t + total - dist[b]) = dist[b] - dist[a] - t.
      ;;
      ;; We need dist[b] - dist[a] - t >= 100 and a cheat duration t <= 20 and t >= manhattan(a,b).
      ;; For fixed a and b, best saving is with smallest possible t = manhattan(a,b).
      ;; If that best saving already meets threshold, then it counts (and if it doesn't, no larger t helps).
      (let [track (filter #(passable? grid %) (keys grid))
            track-set (into #{} track)]
        (loop [i 0
               ans 0]
          (if (= i (count track))
            ans
            (let [a (nth track i)
                  da (dist a)]
              ;; iterate b candidates by manhattan distance <= 20
              ;; bounds via offsets
              (let [ans2
                    (reduce
                     (fn [acc [dx dy]]
                       (let [b [(+ (first a) dx) (+ (second a) dy)]
                             mb (get dist b)]
                         (if (nil? mb)
                           acc
                           (let [md (+ (Math/abs dx) (Math/abs dy))
                                 saved (- mb da md)]
                             (if (>= saved threshold)
                               (inc acc)
                               acc)))))
                     ans offsets)]
                (recur (inc i) ans2)))))))))

(println (solve))
