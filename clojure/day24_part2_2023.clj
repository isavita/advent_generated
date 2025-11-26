
(ns stone
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-longs [line]
  (map #(BigInteger. %) (re-seq #"-?\d+" line)))

(defn make-vec [[x y z]] {:x x :y y :z z})

(defn sub-vec [a b]
  {:x (- (:x a) (:x b))
   :y (- (:y a) (:y b))
   :z (- (:z a) (:z b))})

(defn add-vec [a b]
  {:x (+ (:x a) (:x b))
   :y (+ (:y a) (:y b))
   :z (+ (:z a) (:z b))})

(defn mul-vec [v s]
  {:x (* (:x v) s)
   :y (* (:y v) s)
   :z (* (:z v) s)})

(defn div-vec [v s]
  {:x (quot (:x v) s)
   :y (quot (:y v) s)
   :z (quot (:z v) s)})

(defn dot [a b]
  (+ (* (:x a) (:x b))
     (* (:y a) (:y b))
     (* (:z a) (:z b))))

(defn cross [a b]
  {:x (- (* (:y a) (:z b)) (* (:z a) (:y b)))
   :y (- (* (:z a) (:x b)) (* (:x a) (:z b)))
   :z (- (* (:x a) (:y b)) (* (:y a) (:x b)))})

(defn intersection-time [r s]
  (let [plane (cross (:p r) (add-vec (:p r) (:v r)))]
    (quot (* -1 (dot (:p s) plane))
          (dot (:v s) plane))))

(defn solve [input]
  (let [stones (map (fn [line]
                    (let [[px py pz vx vy vz] (parse-longs line)]
                      {:p (make-vec [px py pz])
                       :v (make-vec [vx vy vz])}))
                  (take 3 input))
        s1 (second stones)
        s2 (nth stones 2)
        ref1 {:p (sub-vec (:p s1) (:p (first stones)))
              :v (sub-vec (:v s1) (:v (first stones)))}
        ref2 {:p (sub-vec (:p s2) (:p (first stones)))
              :v (sub-vec (:v s2) (:v (first stones)))}
        t1 (intersection-time ref2 ref1)
        t2 (intersection-time ref1 ref2)
        rock1 (add-vec (:p s1) (mul-vec (:v s1) t1))
        rock2 (add-vec (:p s2) (mul-vec (:v s2) t2))
        rp (add-vec rock1 (mul-vec (div-vec (sub-vec rock2 rock1) (- t2 t1)) (- t1)))]
    (str (+ (:x rp) (:y rp) (:z rp)))))

(defn -main []
  (let [input (line-seq (io/reader "input.txt"))]
    (println (solve input))))

(-main)
