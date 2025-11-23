
(ns robot-keypad
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def key-pad ["789" "456" "123" " 0A"])
(def robot-pad [" ^A" "<v>"])

(defn find-pos [mat ch]
  (first (for [i (range (count mat))
               j (range (count (nth mat i)))
               :when (= (nth (nth mat i) j) ch)]
           [i j])))

(defn ok [mat [curr-i curr-j] seq]
  (let [mat-rows (count mat)
        mat-cols (count (nth mat 0))]
    (loop [curr-i curr-i curr-j curr-j seq seq]
      (if (empty? seq)
        true
        (let [ch (first seq)]
          (cond
            (not (and (< -1 curr-i mat-rows)
                      (< -1 curr-j mat-cols)
                      (not= (nth (nth mat curr-i) curr-j) \space))) false
            (= ch \^) (recur (dec curr-i) curr-j (rest seq))
            (= ch \v) (recur (inc curr-i) curr-j (rest seq))
            (= ch \<) (recur curr-i (dec curr-j) (rest seq))
            (= ch \>) (recur curr-i (inc curr-j) (rest seq))
            :else (recur curr-i curr-j (rest seq))))))))

(defn generate-moves [position objective pad]
  (let [[obj-i obj-j] (find-pos pad objective)
        [pos-i pos-j] position]
    (let [attempt1 (str (apply str (repeat (- pos-j obj-j) \<))
                        (apply str (repeat (- pos-i obj-i) \^))
                        (apply str (repeat (- obj-i pos-i) \v))
                        (apply str (repeat (- obj-j pos-j) \>)))]
      (if (ok pad position attempt1)
        attempt1
        (str (apply str (repeat (- obj-j pos-j) \>))
             (apply str (repeat (- pos-i obj-i) \^))
             (apply str (repeat (- obj-i pos-i) \v))
             (apply str (repeat (- pos-j obj-j) \<)))))))

(def memo (atom {}))

(defn solve [code robots max-robots]
  (let [key [code robots]]
    (if (contains? @memo key)
      (@memo key)
      (let [result
            (if (<= robots 0)
              (count code)
              (loop [current-robot-pos (if (= robots max-robots) [3 2] [0 2])
                     code code
                     ret 0]
                (if (empty? code)
                  ret
                  (let [ch (first code)
                        current-pad (if (= robots max-robots) key-pad robot-pad)
                        moves (generate-moves current-robot-pos ch current-pad)
                        new-pos (find-pos current-pad ch)]
                    (recur new-pos (rest code) (+ ret (solve (str moves "A") (dec robots) max-robots)))))))]
        (do (swap! memo assoc key result)
            result)))))

(defn main []
  (let [max-robots 26
        input-lines (with-open [rdr (io/reader "input.txt")]
                      (doall (line-seq rdr)))
        total-ret (atom 0)]
    (doseq [line input-lines]
      (let [trimmed (str/trim line)]
        (when-not (empty? trimmed)
          (let [numeric-part (Long/parseLong (apply str (filter #(Character/isDigit %) trimmed)))]
            (swap! total-ret + (* (solve trimmed max-robots max-robots) numeric-part))))))
    (println @total-ret)))

(main)
