
(ns snailfish
  (:require [clojure.java.io :as io]))

(defrecord SnailNumber [value left right])

(defn regular? [node]
  (and (nil? (:left node)) (nil? (:right node))))

(defn add-left [node v]
  (if (regular? node)
    (SnailNumber. (+ (:value node) v) nil nil)
    (SnailNumber. (:value node) (add-left (:left node) v) (:right node))))

(defn add-right [node v]
  (if (regular? node)
    (SnailNumber. (+ (:value node) v) nil nil)
    (SnailNumber. (:value node) (:left node) (add-right (:right node) v))))

(defn explode
  ([node] (explode node 0))
  ([node depth]
   (if (regular? node)
     [false 0 0 node]
     (if (= depth 4)
       [true (:value (:left node)) (:value (:right node)) (SnailNumber. 0 nil nil)]
       (let [[exploded-left lval rval new-left] (explode (:left node) (inc depth))]
         (if exploded-left
           [true lval 0 (SnailNumber. (:value node) new-left
                                      (if (pos? rval) (add-left (:right node) rval) (:right node)))]
           (let [[exploded-right lval rval new-right] (explode (:right node) (inc depth))]
             (if exploded-right
               [true 0 rval (SnailNumber. (:value node)
                                         (if (pos? lval) (add-right (:left node) lval) (:left node))
                                         new-right)]
               [false 0 0 node]))))))))

(defn split [node]
  (cond
    (regular? node)
    (if (>= (:value node) 10)
      [(SnailNumber. -1
                     (SnailNumber. (quot (:value node) 2) nil nil)
                     (SnailNumber. (Math/ceil (/ (:value node) 2)) nil nil))
       true]
      [node false])
    :else
    (let [[new-left splitted] (split (:left node))]
      (if splitted
        [(SnailNumber. (:value node) new-left (:right node)) true]
        (let [[new-right splitted] (split (:right node))]
          [(SnailNumber. (:value node) (:left node) new-right) splitted])))))

(defn reduce-once [node]
  (let [[exploded _ _ new-node] (explode node)]
    (if exploded
      new-node
      (let [[new-node splitted] (split node)]
        (if splitted new-node node)))))

(defn reduce-snail [node]
  (loop [current node]
    (let [next (reduce-once current)]
      (if (= current next) current (recur next)))))

(defn add [a b]
  (reduce-snail (SnailNumber. -1 a b)))

(defn parse-snail [s]
  (let [s (clojure.string/trim s)]
    (if (= (first s) \[)
      (let [body (subs s 1 (dec (count s)))
            [left right] (loop [i 0 balance 0]
                           (cond
                             (= (nth body i) \,) (if (zero? balance)
                                                    [(subs body 0 i) (subs body (inc i))]
                                                    (recur (inc i) balance))
                             (= (nth body i) \[) (recur (inc i) (inc balance))
                             (= (nth body i) \]) (recur (inc i) (dec balance))
                             :else (recur (inc i) balance)))]
        (SnailNumber. -1 (parse-snail left) (parse-snail right)))
      (SnailNumber. (Long/parseLong s) nil nil))))

(defn magnitude [node]
  (if (regular? node)
    (:value node)
    (+ (* 3 (magnitude (:left node))) (* 2 (magnitude (:right node))))))

(defn -main [& _]
  (let [lines (line-seq (io/reader "input.txt"))
        nums (mapv parse-snail lines)]
    (if (empty? nums)
      (println "No snailfish numbers found in the file.")
      (println (magnitude (reduce add nums))))))

(-main)
