(ns day20
  (:require [clojure.string :as str]
             [clojure.java.io :as io]))

(defn read-input []
  (with-open [r (io/reader "input.txt")]
    (first (line-seq r))))

(defn parse-regex [regex]
  (let [stack (atom [])
        distances (atom {})
        x (atom 0)
        y (atom 0)
        distance (atom 0)]
    (swap! distances assoc [0 0] 0)
    (doseq [c (seq regex)]
      (cond
        (= c \N) (do (swap! y dec) (swap! distance inc))
        (= c \S) (do (swap! y inc) (swap! distance inc))
        (= c \E) (do (swap! x inc) (swap! distance inc))
        (= c \W) (do (swap! x dec) (swap! distance inc))
        (= c \() (do (swap! stack conj [@x @y @distance]))
        (= c \|) (let [[nx ny nd] (peek @stack)]
                   (reset! x nx)
                   (reset! y ny)
                   (reset! distance nd))
        (= c \)) (do (swap! stack pop))
        :else nil)
      (let [pos [@x @y]]
        (if (or (not (contains? @distances pos))
                (< @distance (@distances pos)))
          (swap! distances assoc pos @distance))))
    @distances))

(defn solve []
  (let [regex (subs (read-input) 1 (dec (count (read-input))))
        distances (parse-regex regex)]
    (count (filter #(>= % 1000) (vals distances)))))

(println (solve))