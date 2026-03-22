
(ns main
  (:gen-class)
  (:require [clojure.string :as str]))

(def dirs [[-1 0 :left] [0 -1 :up] [1 0 :right] [0 1 :down]])

(defn save-outer [side label x y]
  (let [key (if (or (= label :up) (= label :down))
              (str y ":" x)
              (str x ":" y))]
    (update side label (fnil conj #{}) key)))

(defn count-outer [side]
  (reduce
   (fn [outer [_ s]]
     (let [arr (sort-by (fn [v]
                          (let [[a b] (mapv parse-long (str/split v #":"))]
                            [a b]))
                        s)]
       (loop [temp #{}
              xs arr
              cnt outer]
         (if (empty? xs)
           cnt
           (let [[i j] (mapv parse-long (str/split (first xs) #":"))
                 found? (or (contains? temp (str i ":" (dec j)))
                            (contains? temp (str i ":" (inc j))))]
             (recur (conj temp (first xs))
                    (rest xs)
                    (if found? cnt (inc cnt))))))))
   0
   side))

(defn -main []
  (let [lines (-> (slurp "input.txt")
                  str/split-lines
                  (->> (remove str/blank?))
                  vec)
        h (count lines)
        w (count (first lines))
        grid (atom (vec (mapv vec lines)))
        sum (atom 0)]
    (doseq [y (range h)
            x (range w)]
      (when (not= \. (get-in @grid [y x]))
        (let [target (get-in @grid [y x])
              visited (atom #{})
              side (atom {})
              area (atom 0)]
          (letfn [(search [cx cy label]
                    (if (not= (get-in @grid [cy cx]) target)
                      (when (and (not= label "") (not (contains? @visited [cx cy])))
                        (swap! side save-outer label cx cy))
                      (do
                        (swap! visited conj [cx cy])
                        (swap! area inc)
                        (swap! grid assoc-in [cy cx] \.)
                        (doseq [[dx dy lab] dirs
                                :let [nx (+ cx dx)
                                      ny (+ cy dy)]]
                          (if (or (< nx 0) (>= nx w) (< ny 0) (>= ny h))
                            (swap! side save-outer lab nx ny)
                            (search nx ny lab))))))]
            (search x y nil)
            (swap! sum + (* @area (count-outer @side)))))))
    (println @sum)))

(-main)
