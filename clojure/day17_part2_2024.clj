
(ns regolith-resurrection
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input []
  (with-open [rdr (io/reader "input.txt")]
    (let [lines (str/split-lines (str/trim (slurp rdr)))]
      (loop [a nil b nil c nil program nil ls lines]
        (if (empty? ls)
          {:a a :b b :c c :program program}
          (let [line (first ls)]
            (cond
              (str/starts-with? line "Register A:")
              (recur (Long/parseLong (str/trim (second (str/split line #":")))) b c program (rest ls))
              (str/starts-with? line "Register B:")
              (recur a (Long/parseLong (str/trim (second (str/split line #":")))) c program (rest ls))
              (str/starts-with? line "Register C:")
              (recur a b (Long/parseLong (str/trim (second (str/split line #":")))) program (rest ls))
              (str/starts-with? line "Program:")
              (recur a b c (mapv #(Long/parseLong (str/trim %)) (str/split (str/trim (second (str/split line #":"))) #",")) (rest ls))
              :else (recur a b c program (rest ls)))))))))

(defn combo [val a b c]
  (case val
    0 0
    1 1
    2 2
    3 3
    4 a
    5 b
    6 c
    (throw (ex-info "Invalid combo operand" {:val val}))))

(defn run-program [a b c program]
  (loop [a a b b c c ip 0 outs []]
    (if (>= ip (count program))
      outs
      (let [cmd (program ip) lit (program (inc ip))
            jump nil
            [a b c outs]
            (case cmd
              0 [(bit-shift-right a (combo lit a b c)) b c outs]
              1 [a (bit-xor b lit) c outs]
              2 [a (rem (combo lit a b c) 8) c outs]
              3 (if (zero? a)
                  [a b c outs]
                  (do (def jump lit) [a b c outs]))
              4 [a (bit-xor b c) c outs]
              5 [a b c (conj outs (rem (combo lit a b c) 8))]
              6 [a (bit-shift-right a (combo lit a b c)) c outs]
              7 [a b (bit-shift-right a (combo lit a b c)) outs]
              (throw (ex-info "Invalid opcode" {:cmd cmd})))]
        (if jump
          (recur a b c jump outs)
          (recur a b c (+ 2 ip) outs))))))

(defn find-min-a [{:keys [b c program]}]
  (let [target (reverse program)]
    (loop [candidates [0] depth 0]
      (if (= depth (count program))
        (first candidates)
        (let [next-cands
              (for [prefix candidates
                    i (range 8)
                    :let [a (+ i (* 8 prefix))
                          outs (run-program a b c program)
                          outs (reverse outs)]
                    :when (= (first outs) (nth target depth))]
                a)]
          (recur (sort next-cands) (inc depth)))))))

(defn -main []
  (let [input (parse-input)]
    (println (find-min-a input))))

(-main)
