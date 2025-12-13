
(ns safe-cracker
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn floor-div [a b]
  (let [q (quot a b)]
    (if (and (< a 0) (not (zero? (rem a b))))
      (dec q)
      q)))

(defn parse-line [line]
  (let [dir (first line)
        n   (Long/parseUnsignedLong (subs line 1))]
    (case dir
      \R [:right n]
      \L [:left  n]
      (throw (ex-info "Bad direction" {:dir dir})))))

(defn count-hits [file]
  (with-open [rdr (io/reader file)]
    (loop [pos 50
           hits 0
           lines (line-seq rdr)]
      (if (empty? lines)
        hits
        (let [line (str/trim (first lines))]
          (if (empty? line)
            (recur pos hits (rest lines))
            (let [[dir n] (parse-line line)]
              (case dir
                :right (let [new-pos (rem (+ pos n) 100)
                             new-hits (+ hits (quot (+ pos n) 100))]
                         (recur new-pos new-hits (rest lines)))
                :left  (let [new-pos (mod (- pos n) 100)
                             new-hits (+ hits (- (floor-div (dec pos) 100)
                                                 (floor-div (dec (- pos n)) 100)))]
                         (recur new-pos new-hits (rest lines)))))))))))

(defn -main [& _]
  (println "The password is:" (count-hits "input.txt")))

(-main)
