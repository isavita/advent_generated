(ns ssl-support
  (:require [clojure.string :as str]
             [clojure.java.io :as io]))

(defn find-abas [s]
  (for [i (range (- (count s) 2))
        :when (and (not= (nth s i) (nth s (inc i)))
                   (= (nth s i) (nth s (+ i 2))))]
    (subs s i (+ i 3))))

(defn supports-ssl [ip]
  (let [inside-brackets (re-seq #"\[[a-z]+\]" ip)
        ip (str/replace ip #"\[[a-z]+\]" "-")]
    (some (fn [aba]
            (let [bab (str (nth aba 1) (nth aba 0) (nth aba 1))]
              (some #(str/index-of % bab) inside-brackets)))
          (find-abas ip))))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [ssl-count (count (filter supports-ssl (line-seq r)))]
      (println ssl-count))))

(-main)