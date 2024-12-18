
(ns space-police
  (:require [clojure.string :as str]))

(defn parse-program [input]
  (->> (str/split input #",")
       (map #(Long/parseLong %))
       (vec)))

(defn get-value [program mode address relative-base]
  (case mode
    0 (get program (get program address 0) 0)
    1 (get program address 0)
    2 (get program (+ relative-base (get program address 0)) 0)))

(defn set-value [program mode address relative-base value]
  (let [target-address (case mode
                         0 (get program address 0)
                         2 (+ relative-base (get program address 0)))]
    (assoc program target-address value)))

(defn run-intcode [program input-fn output-fn]
  (loop [program program
         ip 0
         relative-base 0
         input-queue []]
    (if (>= ip (count program))
      {:program program :halted true}
      (let [opcode (mod (get program ip 0) 100)
            mode1 (mod (quot (get program ip 0) 100) 10)
            mode2 (mod (quot (get program ip 0) 1000) 10)
            mode3 (mod (quot (get program ip 0) 10000) 10)]
        (case opcode
          1 (let [val1 (get-value program mode1 (inc ip) relative-base)
                  val2 (get-value program mode2 (+ ip 2) relative-base)
                  new-program (set-value program mode3 (+ ip 3) relative-base (+ val1 val2))]
              (recur new-program (+ ip 4) relative-base input-queue))
          2 (let [val1 (get-value program mode1 (inc ip) relative-base)
                  val2 (get-value program mode2 (+ ip 2) relative-base)
                  new-program (set-value program mode3 (+ ip 3) relative-base (* val1 val2))]
              (recur new-program (+ ip 4) relative-base input-queue))
          3 (if (empty? input-queue)
              (let [input (input-fn)]
                (recur (set-value program mode1 (inc ip) relative-base input)
                       (+ ip 2) relative-base input-queue))
              (let [input (first input-queue)]
                (recur (set-value program mode1 (inc ip) relative-base input)
                       (+ ip 2) relative-base (rest input-queue))))
          4 (let [output (get-value program mode1 (inc ip) relative-base)]
              (output-fn output)
              (recur program (+ ip 2) relative-base input-queue))
          5 (let [val1 (get-value program mode1 (inc ip) relative-base)
                  val2 (get-value program mode2 (+ ip 2) relative-base)]
              (recur program (if (not= val1 0) val2 (+ ip 3)) relative-base input-queue))
          6 (let [val1 (get-value program mode1 (inc ip) relative-base)
                  val2 (get-value program mode2 (+ ip 2) relative-base)]
              (recur program (if (= val1 0) val2 (+ ip 3)) relative-base input-queue))
          7 (let [val1 (get-value program mode1 (inc ip) relative-base)
                  val2 (get-value program mode2 (+ ip 2) relative-base)
                  new-program (set-value program mode3 (+ ip 3) relative-base (if (< val1 val2) 1 0))]
              (recur new-program (+ ip 4) relative-base input-queue))
          8 (let [val1 (get-value program mode1 (inc ip) relative-base)
                  val2 (get-value program mode2 (+ ip 2) relative-base)
                  new-program (set-value program mode3 (+ ip 3) relative-base (if (= val1 val2) 1 0))]
              (recur new-program (+ ip 4) relative-base input-queue))
          9 (let [val1 (get-value program mode1 (inc ip) relative-base)]
              (recur program (+ ip 2) (+ relative-base val1) input-queue))
          99 {:program program :halted true :ip ip :relative-base relative-base})))))

(defn solve []
  (let [program (-> (slurp "input.txt") parse-program)
        panels (atom {})
        position (atom [0 0])
        direction (atom [0 1]) ; Up
        output-buffer (atom [])
        input-fn (fn []
                   (get @panels @position 0))
        output-fn (fn [output]
                    (swap! output-buffer conj output)
                    (when (= (count @output-buffer) 2)
                      (let [[color turn] @output-buffer]
                        (swap! panels assoc @position color)
                        (swap! output-buffer (constantly []))
                        (swap! direction
                               (fn [dir]
                                 (case turn
                                   0 (case dir
                                       [0 1] [-1 0]
                                       [-1 0] [0 -1]
                                       [0 -1] [1 0]
                                       [1 0] [0 1])
                                   1 (case dir
                                       [0 1] [1 0]
                                       [1 0] [0 -1]
                                       [0 -1] [-1 0]
                                       [-1 0] [0 1]))))
                        (swap! position
                               (fn [[x y]]
                                 [(+ x (first @direction))
                                  (+ y (second @direction))])))))]
    (run-intcode program input-fn output-fn)
    (count @panels)))

(println (solve))
