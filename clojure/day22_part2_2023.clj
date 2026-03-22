
(ns main
  (:require [clojure.string :as str]))

(defn parse-line [line id]
  (let [[a b] (str/split line #"\~")
        [x1 y1 z1] (map #(Long/parseLong %) (str/split a #","))
        [x2 y2 z2] (map #(Long/parseLong %) (str/split b #","))
        mini [(min x1 x2) (min y1 y2) (min z1 z2)]
        maxi [(max x1 x2) (max y1 y2) (max z1 z2)]]
    {:id id
     :mini mini
     :maxi maxi
     :based-on []
     :support []}))

(defn overlaps? [b1 b2]
  (let [[x1 y1] (:mini b1)
        [x2 y2] (:mini b2)
        [X1 Y1] (:maxi b1)
        [X2 Y2] (:maxi b2)]
    (and (<= (max x1 x2) (min X1 X2))
         (<= (max y1 y2) (min Y1 Y2)))))

(defn settle [bricks]
  (let [bricks (vec (sort-by (comp last :mini) bricks))
        n (count bricks)]
    (loop [i 0 b bricks]
      (if (= i n)
        b
        (let [brick (nth b i)
              z1 (nth (:mini brick) 2)
              z2 (nth (:maxi brick) 2)
              delta (- z2 z1)
              [support-z bases]
              (loop [j 0 sz 0 bases []]
                (if (= j i)
                  [sz bases]
                  (let [below (nth b j)]
                    (if (overlaps? brick below)
                      (let [bz (nth (:maxi below) 2)]
                        (cond
                          (> bz sz) (recur (inc j) bz [j])
                          (= bz sz) (recur (inc j) sz (conj bases j))
                          :else (recur (inc j) sz bases)))
                      (recur (inc j) sz bases)))))]
          (let [b2 (-> b
                       (assoc-in [i :based-on] bases)
                       (assoc-in [i :mini 2] (inc support-z))
                       (assoc-in [i :maxi 2] (+ (inc support-z) delta)))]
            (recur (inc i)
                   (reduce (fn [vv bi]
                             (update-in vv [bi :support] conj i))
                           b2
                           bases))))))))

(defn solve [filename]
  (let [lines (str/split-lines (slurp filename))
        bricks (settle (vec (map-indexed (fn [i line] (parse-line line (inc i))) lines)))
        supports (mapv :support bricks)
        based-ons (mapv :based-on bricks)
        n (count bricks)]
    (loop [i 0 total 0]
      (if (= i n)
        total
        (let [falling (boolean-array n)]
          (loop [q (java.util.ArrayDeque.)]
            (doseq [s (supports i)]
              (when (= 1 (count (based-ons s)))
                (when-not (aget falling s)
                  (aset-boolean falling s true)
                  (.add q s))))
            (loop []
              (when-let [cur (.poll q)]
                (doseq [s (supports cur)]
                  (when-not (aget falling s)
                    (when (every? #(or (= % i) (aget falling %)) (based-ons s))
                      (aset-boolean falling s true)
                      (.add q s))))
                (recur))))
          (let [cnt (loop [j 0 c 0]
                      (if (= j n)
                        c
                        (recur (inc j) (if (aget falling j) (inc c) c))))]
            (recur (inc i) (+ total cnt))))))))

(defn -main []
  (println (solve "input.txt")))

(-main)
