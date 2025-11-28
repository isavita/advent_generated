
(ns fractal
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-rules [file]
  (into {} (with-open [rdr (io/reader file)]
             (doall (for [line (line-seq rdr)]
                      (let [[k v] (str/split line #" => ")]
                        [k v]))))))

(defn rotate [pat]
  (let [rows (str/split pat #"/")
        n (count rows)]
    (str/join "/"
              (for [x (range n)]
                (apply str (for [y (range (dec n) -1 -1)]
                             (nth (nth rows y) x)))))))

(defn flip [pat]
  (str/join "/" (map str/reverse (str/split pat #"/"))))

(defn enhance [pat rules]
  (or (rules pat)
      (some (fn [p] (rules p))
            (take 4 (rest (iterate rotate pat))))
      (let [f (flip pat)]
        (or (rules f)
            (some (fn [p] (rules p))
                  (take 4 (rest (iterate rotate f)))))))

(defn split-grid [grid size]
  (let [n (count grid)
        step (if (zero? (mod n 2)) 2 3)]
    (for [y (range 0 n step)
          x (range 0 n step)]
      (str/join "/" (map #(subs % x (+ x step)) (subvec grid y (+ y step)))))))

(defn join-grids [grids size]
  (let [side (int (Math/sqrt (count grids)))
        rows (for [y (range side)]
               (apply map str (for [x (range side)]
                                (str/split (nth grids (+ x (* y side))) #"/"))))]
    (mapcat identity rows)))

(defn step [grid rules]
  (let [size (count grid)
        subsize (if (zero? (mod size 2)) 2 3)
        grids (split-grid grid subsize)
        new-grids (map #(enhance % rules) grids)]
    (join-grids new-grids (inc subsize))))

(defn count-on [grid]
  (count (filter #{\#} (apply str grid))))

(defn -main [& _]
  (let [rules (read-rules "input.txt")
        start [".#." "..#" "###"]]
    (println (count-on (nth (iterate #(step % rules) start) 5)))))

(-main)
