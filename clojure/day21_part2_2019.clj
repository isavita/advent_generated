
(ns springdroid
  (:require [clojure.string :as str]))

(defn load-program [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (let [line (first (line-seq rdr))]
      (->> (str/split line #",")
           (map #(Long/parseLong %))
           vec))))

(defn run-vm [program input]
  (let [mem (atom (into {} (map-indexed vector program)))
        ip (atom 0)
        rb (atom 0)
        inq (atom input)
        out (atom [])]
    (letfn [(get-val [addr] (get @mem addr 0))
            (set-val [addr val] (swap! mem assoc addr val))
            (param-val [idx mode]
              (let [v (get-val (+ @ip idx))]
                (case mode
                  0 (get-val v)
                  1 v
                  2 (get-val (+ @rb v)))))
            (write-addr [idx mode]
              (let [v (get-val (+ @ip idx))]
                (case mode
                  0 v
                  2 (+ @rb v))))
            (step []
              (let [inst (get-val @ip)
                    op (mod inst 100)
                    m1 (mod (quot inst 100) 10)
                    m2 (mod (quot inst 1000) 10)
                    m3 (mod (quot inst 10000) 10)]
                (case op
                  99 nil
                  1 (do (set-val (write-addr 3 m3)
                                 (+ (param-val 1 m1) (param-val 2 m2)))
                        (swap! ip + 4) true)
                  2 (do (set-val (write-addr 3 m3)
                                 (* (param-val 1 m1) (param-val 2 m2)))
                        (swap! ip + 4) true)
                  3 (if (empty? @inq)
                      false
                      (do (set-val (write-addr 1 m1) (first @inq))
                          (swap! inq rest)
                          (swap! ip + 2) true))
                  4 (do (swap! out conj (param-val 1 m1))
                        (swap! ip + 2) true)
                  5 (if (not= (param-val 1 m1) 0)
                     (do (reset! ip (param-val 2 m2)) true)
                     (do (swap! ip + 3) true))
                  6 (if (= (param-val 1 m1) 0)
                     (do (reset! ip (param-val 2 m2)) true)
                     (do (swap! ip + 3) true))
                  7 (do (set-val (write-addr 3 m3)
                                 (if (< (param-val 1 m1) (param-val 2 m2)) 1 0))
                        (swap! ip + 4) true)
                  8 (do (set-val (write-addr 3 m3)
                                 (if (= (param-val 1 m1) (param-val 2 m2)) 1 0))
                        (swap! ip + 4) true)
                  9 (do (swap! rb + (param-val 1 m1))
                        (swap! ip + 2) true)
                  false)))]
      (while (step))
      @out)))

(defn send-str [s]
  (mapv int (str s "\n")))

(defn -main []
  (let [program (load-program "input.txt")
        instructions ["NOT A J"
                      "NOT B T"
                      "OR T J"
                      "NOT C T"
                      "OR T J"
                      "AND D J"
                      "NOT A T"
                      "AND A T"
                      "OR E T"
                      "OR H T"
                      "AND T J"
                      "RUN"]
        input (vec (mapcat send-str instructions))
        output (run-vm program input)]
    (doseq [v output]
      (if (and (>= v 0) (<= v 127))
        (print (char v))
        (println v)))))

(-main)
