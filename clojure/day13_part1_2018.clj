
(ns day13
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        carts (atom [])
        tracks (vec (for [y (range height)]
                      (vec (for [x (range width)]
                             (let [c (get-in lines [y x])]
                               (case c
                                 \^ (do (swap! carts conj {:x x :y y :dir :up :turn :left}) \|)
                                 \v (do (swap! carts conj {:x x :y y :dir :down :turn :left}) \|)
                                 \< (do (swap! carts conj {:x x :y y :dir :left :turn :left}) \-)
                                 \> (do (swap! carts conj {:x x :y y :dir :right :turn :left}) \-)
                                 c))))))]
    {:tracks tracks :carts carts}))

(defn move-cart [tracks cart]
  (let [{:keys [x y dir turn]} cart
        next-pos (case dir
                   :up {:x x :y (dec y)}
                   :down {:x x :y (inc y)}
                   :left {:x (dec x) :y y}
                   :right {:x (inc x) :y y})
        next-x (:x next-pos)
        next-y (:y next-pos)
        next-track (get-in tracks [next-y next-x])]
    (cond
      (= next-track \+)
      (let [next-dir (case turn
                       :left (case dir :up :left :down :right :left :down :right :up)
                       :straight dir
                       :right (case dir :up :right :down :left :left :up :right :down))
            next-turn (case turn :left :straight :straight :right :right :left)]
        (assoc cart :x next-x :y next-y :dir next-dir :turn next-turn))
      (= next-track \/)
      (let [next-dir (case dir :up :right :down :left :left :down :right :up)]
        (assoc cart :x next-x :y next-y :dir next-dir))
      (= next-track \\)
      (let [next-dir (case dir :up :left :down :right :left :up :right :down)]
        (assoc cart :x next-x :y next-y :dir next-dir))
      :else
      (assoc cart :x next-x :y next-y))))

(defn tick [tracks carts]
  (let [sorted-carts (sort-by (juxt :y :x) @carts)
        moved-carts (atom [])
        collisions (atom nil)]
    (doseq [cart sorted-carts]
      (when (not @collisions)
        (let [moved-cart (move-cart tracks cart)
              collided (some (fn [c] (and (= (:x c) (:x moved-cart)) (= (:y c) (:y moved-cart)))) @moved-carts)]
          (if collided
            (reset! collisions {:x (:x moved-cart) :y (:y moved-cart)})
            (swap! moved-carts conj moved-cart)))))
    (reset! carts @moved-carts)
    @collisions))

(defn solve [input]
  (let [{:keys [tracks carts]} (parse-input input)]
    (loop [tick-num 0]
      (if-let [collision (tick tracks carts)]
        collision
        (recur (inc tick-num))))))

(defn -main []
  (let [input (slurp "input.txt")
        collision (solve input)]
    (println (str (:x collision) "," (:y collision)))))

(-main)
