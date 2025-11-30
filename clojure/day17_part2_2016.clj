
(ns advent
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.security MessageDigest]))

(defn md5 [s]
  (let [digest (MessageDigest/getInstance "MD5")]
    (.update digest (.getBytes s "UTF-8"))
    (format "%032x" (BigInteger. 1 (.digest digest)))))

(defn open-doors [pass path]
  (let [h (md5 (str pass path))]
    (cond-> []
      (<= \b (nth h 0) \f) (conj \U)
      (<= \b (nth h 1) \f) (conj \D)
      (<= \b (nth h 2) \f) (conj \L)
      (<= \b (nth h 3) \f) (conj \R))))

(defn longest-path [pass]
  (loop [q [[0 0 ""]], longest 0]
    (if (empty? q)
      longest
      (let [[x y path] (peek q)
            q (pop q)]
        (if (and (= x 3) (= y 3))
          (recur q (max longest (count path)))
          (recur (into q (keep (fn [dir]
                                 (let [[dx dy] (case dir
                                                 \U [0 -1]
                                                 \D [0 1]
                                                 \L [-1 0]
                                                 \R [1 0])
                                       nx (+ x dx)
                                       ny (+ y dy)]
                                   (when (and (< -1 nx 4) (< -1 ny 4))
                                     [nx ny (str path dir)])))
                               (open-doors pass path)))
                 longest))))))

(defn -main [& _]
  (let [pass (str/trim (slurp "input.txt"))]
    (println (longest-path pass))))

(-main)
