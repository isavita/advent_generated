
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def ^:const empty-cell \.)
(def ^:const wall \#)
(def ^:const north-slopes \^)
(def ^:const south-slopes \v)
(def ^:const west-slopes \<)
(def ^:const east-slopes \>)

(def directions
  {:north [0 -1]
   :south [0 1]
   :west [-1 0]
   :east [1 0]})

(def slope-to-dir
  {north-slopes :north
   south-slopes :south
   west-slopes :west
   east-slopes :east})

(defn parse-input [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))]
    {:width width
     :height height
     :data (into {}
                 (for [y (range height)
                       x (range width)
                       :let [c (nth (nth lines y) x)]
                       :when (not= c empty-cell)]
                   [[x y] c]))}))

(defn in-bounds? [grid [x y]]
  (and (<= 0 x (dec (:width grid)))
       (<= 0 y (dec (:height grid)))))

(defn valid-neighbor? [grid pos dir]
  (and (in-bounds? grid pos)
       (not= (get-in grid [:data pos]) wall)))

(defn valid-neighbor-with-slopes? [grid pos dir]
  (and (in-bounds? grid pos)
       (let [cell (get-in grid [:data pos])]
         (if cell
           (and (not= cell wall)
                (= (slope-to-dir cell) dir))
           true))))

(defn neighbors4 [grid coord is-valid?]
  (for [[dir-key [dx dy]] directions
        :let [neighbor [(+ (first coord) dx) (+ (second coord) dy)]]
        :when (is-valid? grid neighbor dir-key)]
    neighbor))

(defn get-graph [grid start end is-valid?]
  (let [vertices (atom #{start end})
        edges (atom {})]
    ;; Find junctions
    (doseq [y (range (:height grid))
            x (range (:width grid))
            :let [coord [x y]]
            :when (and (not (get-in grid [:data coord]))
                       (> (count (neighbors4 grid coord valid-neighbor?)) 2))]
      (swap! vertices conj coord))
    
    ;; Build edges between junctions
    (doseq [start-vertex @vertices]
      (let [edges-from-start (atom {})]
        (loop [frontier [[start-vertex 0]]
               reached #{start-vertex}]
          (when (seq frontier)
            (let [[current dist] (first frontier)
                  rest-frontier (rest frontier)]
              (if (and (not= current start-vertex)
                       (@vertices current))
                (do
                  (swap! edges-from-start assoc {:start start-vertex
                                                 :end current
                                                 :weight dist}
                         true)
                  (recur rest-frontier reached))
                (let [next-steps (for [next (neighbors4 grid current is-valid?)
                                      :when (not (reached next))]
                                  [next (inc dist)])
                      new-reached (into reached (map first next-steps))]
                  (recur (concat rest-frontier next-steps) new-reached))))))
        (swap! edges assoc start-vertex @edges-from-start)))
    {:vertices @vertices :edges @edges}))

(defn max-distance-dfs [graph current end seen]
  (if (= current end)
    [true 0]
    (let [seen (conj seen current)
          max-dist (atom 0)
          found-path (atom false)]
      (doseq [[edge _] (get-in graph [:edges current])]
        (when (not (seen (:end edge)))
          (let [[valid-path dist] (max-distance-dfs graph (:end edge) end seen)]
            (when valid-path
              (reset! found-path true)
              (reset! max-dist (max @max-dist (+ dist (:weight edge))))))))
      (if @found-path
        [true @max-dist]
        [false 0]))))

(defn solve [input]
  (let [grid (parse-input input)
        start [1 0]
        end [(- (:width grid) 2) (- (:height grid) 1)]
        graph (get-graph grid start end valid-neighbor-with-slopes?)]
    (second (max-distance-dfs graph start end #{}))))

(defn -main []
  (let [input (slurp "input.txt")]
    (println (solve input))))

(-main)
