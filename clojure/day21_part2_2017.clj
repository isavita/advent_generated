
(ns fractal
  (:require [clojure.string :as str]))

(def ^:private memo (atom {}))

(defn- rotate [s]
  (let [lines (str/split s #"/")
        n (count lines)]
  (str/join "/" (mapv (fn [x]
                        (apply str (mapv #(nth (lines %) (- n x 1)) (range n))))
                      (range n)))))

(defn- flip [s]
  (str/join "/" (mapv str/reverse (str/split s #"/"))))

(defn- enhance [input rules]
  (if-let [cached (@memo input)]
    cached
    (let [original input]
      (loop [temp input n 0]
        (cond
          (contains? rules temp) (do (swap! memo assoc original (rules temp))
                                      (rules temp))
          (< n 3) (recur (rotate temp) (inc n))
          (= n 3) (recur (flip original) (inc n))
          (< n 7) (recur (rotate temp) (inc n))
          :else "")))))

(defn- parse-rules [filename]
  (into {} (with-open [rdr (clojure.java.io/reader filename)]
             (doall (for [line (line-seq rdr)
                         :let [[from to] (str/split line #" => ")]]
                      [from to])))))

(defn- break-grid [grid size]
  (for [y (range 0 (count grid) size)
        x (range 0 (count (first grid)) size)]
    (str/join "/" (mapv #(subs % x (+ x size)) (subvec grid y (+ y size))))))

(defn- assemble-grid [squares new-size]
  (let [side (int (Math/sqrt (count squares)))
        rows (partition side squares)]
    (vec (for [row rows
               i (range new-size)]
           (str/join (mapv #(nth (str/split % #"/") i) row))))))

(defn -main [& _]
  (let [rules (parse-rules "input.txt")
        initial [".#." "..#" "###"]]
    (loop [grid initial iter 0]
      (if (= iter 18)
        (println (count (filter #(= % \# ) (str/join grid))))
        (let [size (if (even? (count grid)) 2 3)
              new-size (+ size 1)
              squares (break-grid grid size)
              enhanced (mapv #(enhance % rules) squares)
              new-grid (assemble-grid enhanced new-size)]
          (recur new-grid (inc iter)))))))

(-main)
