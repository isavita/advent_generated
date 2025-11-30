
(ns scramble
  (:require [clojure.string :as str]))

(defn swap-pos [pw x y]
  (assoc pw x (pw y) y (pw x)))

(defn swap-letters [pw x y]
  (let [xi (.indexOf pw x)
        yi (.indexOf pw y)]
    (swap-pos pw xi yi)))

(defn rotate [pw steps]
  (let [l (count pw)
        s (mod (+ (mod steps l) l) l)]
    (vec (concat (drop (- l s) pw) (take (- l s) pw)))))

(defn rotate-letter [pw x]
  (let [idx (.indexOf pw x)
        steps (if (>= idx 4) (+ idx 2) (inc idx))]
    (rotate pw steps)))

(defn derotate-letter [pw x]
  (let [idx (.indexOf pw x)
        rot (cond (odd? idx) (/ (- (+ idx 1)) 2)
                  (pos? idx) (/ (- 6 idx) 2)
                  :else -1)]
    (rotate pw rot)))

(defn reverse-range [pw x y]
  (let [before (subvec pw 0 x)
        middle (vec (reverse (subvec pw x (inc y))))
        after (subvec pw (inc y))]
    (vec (concat before middle after))))

(defn move [pw x y]
  (let [ch (pw x)
        without (vec (concat (subvec pw 0 x) (subvec pw (inc x))))]
    (vec (concat (subvec without 0 y) [ch] (subvec without y)))))

(defn apply-inst [pw inst direction]
  (let [line (str/split inst #" ")
        op (first line)]
    (cond
      (= op "swap")
      (if (= (second line) "position")
        (swap-pos pw (Integer/parseInt (nth line 2)) (Integer/parseInt (nth line 5)))
        (swap-letters pw (nth (nth line 2) 0) (nth (nth line 5) 0)))

      (= op "rotate")
      (if (= (second line) "based")
        (if (pos? direction)
          (rotate-letter pw (nth (nth line 6) 0))
          (derotate-letter pw (nth (nth line 6) 0)))
        (let [x (Integer/parseInt (nth line 2))
              x (if (= (second line) "left") (- x) x)
              x (if (neg? direction) (- x) x)]
          (rotate pw x)))

      (= op "reverse")
      (reverse-range pw (Integer/parseInt (nth line 2)) (Integer/parseInt (nth line 4)))

      (= op "move")
      (let [x (Integer/parseInt (nth line 2))
            y (Integer/parseInt (nth line 5))
            [x y] (if (neg? direction) [y x] [x y])]
        (move pw x y))

      :else pw)))

(defn process [instructions direction]
  (reduce (fn [pw inst] (apply-inst pw inst direction))
          (vec "fbgdceah")
          (if (neg? direction) (reverse instructions) instructions)))

(defn -main []
  (let [instructions (str/split-lines (slurp "input.txt"))]
    (println (str/join (process instructions -1)))))

(-main)
