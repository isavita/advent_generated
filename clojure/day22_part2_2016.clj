
(require '[clojure.string :as str]
         '[clojure.java.io :as io])

(defn parse-input []
  (reduce (fn [acc line]
            (if-let [[_ x y] (re-find #"-x(\d+)-y(\d+)" line)]
              (let [parts (str/split line #"\s+")
                    used (Integer/parseInt (subs (parts 2) 0 (dec (count (parts 2)))))
                    avail (Integer. (subs (parts 3) 0 (dec (count (parts 3)))))]
                (assoc acc [(Integer/parseInt x) (Integer/parseInt y)] {:used used :avail avail}))
              acc)) {} (drop 2 (str/split-lines (slurp "input.txt")))))

(defn dims [nodes]
  (let [xs (map first (keys nodes))
        ys (map second (keys nodes))]
    [(apply max xs) (apply max ys)]))

(defn find-hole [nodes]
  (some (fn [[p {:keys [used]}]] (when (= used 0) p)) nodes))

(def neighbors [[0 1] [0 -1] [1 0] [-1 0]])

(defn moves [nodes w goal-data-pos start-hole target-hole]
  (when (= start-hole target-hole) (throw (ex-info "0 moves" {})))
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY start-hole)
         visited {start-hole 0}]
    (when (empty? q) (throw (ex-info "no path" {})))
    (let [[x y] (peek q)
          depth (visited [x y])]
      (if (= [x y] target-hole)
        depth
        (let [nxt-depth (inc depth)
              children (for [[dx dy] neighbors
                            :let [nx (+ x dx) ny (+ y dy)]
                            :when (and (<= 0 nx w) (<= 0 ny w)
                                       (not= [nx ny] goal-data-pos)
                                       (< (get-in nodes [[nx ny] :used] 1000) 400)
                                       (not (<= (get visited [nx ny] 1000) nxt-depth)))]
                        [nx ny])]
          (recur (into (pop q) children)
                 (reduce (fn [v c] (assoc v c nxt-depth)) visited children)))))))

(defn solve []
  (let [nodes (parse-input)
        [w _] (dims nodes)
        goal-data-pos [w 0]
        hole-pos (find-hole nodes)]
    (loop [goal-pos goal-data-pos hole-pos hole-pos total-moves 0]
      (if (= goal-pos [0 0])
        total-moves
        (let [hole-target-pos [(dec (first goal-pos)) (second goal-pos)]
              m (moves nodes w goal-pos hole-pos hole-target-pos)]
          (recur hole-target-pos goal-pos (+ total-moves m 1)))))))

(println (solve))
