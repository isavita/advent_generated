(ns tls-counter
  (:require [clojure.java.io :as io]
             [clojure.string :as str]))

(defn contains-abba [s]
  (loop [i 0]
    (if (>= i (- (count s) 3))
      false
      (if (and (not= (nth s i) (nth s (inc i)))
               (= (nth s i) (nth s (+ i 3)))
               (= (nth s (inc i)) (nth s (+ i 2))))
        true
        (recur (inc i))))))

(defn supports-tls [ip]
  (let [inside-brackets (re-seq #"\[[a-z]+\]" ip)
        ip (apply str (interpose "-" (remove #(re-find #"\[" %) (str/split ip #"\[|\]"))))]
    (and (not (some contains-abba inside-brackets))
         (contains-abba ip))))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [tls-count (count (filter supports-tls (line-seq r)))]
      (println tls-count))))

(-main)