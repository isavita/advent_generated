
(ns maze-solver
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-maze [filename]
  (->> (slurp filename)
       str/split-lines
       (mapv vec)))

(defn find-portals [maze]
  (let [height (count maze)
        width (count (first maze))
        portals (atom {})
        portal-positions (atom {})]
    (doseq [y (range height)
            x (range width)
            :let [c (get-in maze [y x])]
            :when (Character/isUpperCase c)]
      ;; Horizontal portal
      (when (and (< (inc x) width)
                 (Character/isUpperCase (get-in maze [y (inc x)])))
        (let [portal-name (str c (get-in maze [y (inc x)]))]
          (cond
            (and (< (+ x 2) width) (= \. (get-in maze [y (+ x 2)])))
            (do
              (swap! portals update portal-name (fnil conj []) [(+ x 2) y])
              (swap! portal-positions assoc [(+ x 2) y] portal-name))
            (and (>= (dec x) 0) (= \. (get-in maze [y (dec x)])))
            (do
              (swap! portals update portal-name (fnil conj []) [(dec x) y])
              (swap! portal-positions assoc [(dec x) y] portal-name)))))
      ;; Vertical portal
      (when (and (< (inc y) height)
                 (Character/isUpperCase (get-in maze [(inc y) x])))
        (let [portal-name (str c (get-in maze [(inc y) x]))]
          (cond
            (and (< (+ y 2) height) (= \. (get-in maze [(+ y 2) x])))
            (do
              (swap! portals update portal-name (fnil conj []) [x (+ y 2)])
              (swap! portal-positions assoc [x (+ y 2)] portal-name))
            (and (>= (dec y) 0) (= \. (get-in maze [(dec y) x])))
            (do
              (swap! portals update portal-name (fnil conj []) [x (dec y)])
              (swap! portal-positions assoc [x (dec y)] portal-name))))))
    [@portals @portal-positions]))

(defn bfs [maze portals portal-positions start end]
  (let [queue (atom (conj clojure.lang.PersistentQueue/EMPTY [start 0]))
        visited (atom #{start})]
    (loop []
      (if (empty? @queue)
        -1
        (let [[[x y] steps] (peek @queue)]
          (swap! queue pop)
          (if (= [x y] end)
            steps
            (do
              ;; Explore neighbors
              (doseq [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                      :let [nx (+ x dx)
                            ny (+ y dy)]
                      :when (and (>= nx 0) (< nx (count (first maze)))
                                 (>= ny 0) (< ny (count maze))
                                 (= \. (get-in maze [ny nx]))
                                 (not (contains? @visited [nx ny])))]
                (swap! visited conj [nx ny])
                (swap! queue conj [[nx ny] (inc steps)]))
              ;; Handle portals
              (when-let [portal-name (get portal-positions [x y])]
                (doseq [[px py] (get portals portal-name)
                        :when (and (not= [px py] [x y])
                                   (not (contains? @visited [px py])))]
                  (swap! visited conj [px py])
                  (swap! queue conj [[px py] (inc steps)])))
              (recur))))))))

(defn -main []
  (let [maze (read-maze "input.txt")
        [portals portal-positions] (find-portals maze)
        start (first (get portals "AA"))
        end (first (get portals "ZZ"))
        result (bfs maze portals portal-positions start end)]
    (println result)))

(-main)
