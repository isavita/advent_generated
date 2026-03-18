
(ns solution
  (:require [clojure.string :as str]))

(defn read-input []
  (-> "input.txt" slurp str/split-lines))

(defn parse-int [s]
  (Integer/parseInt s))

(defn is-register? [x]
  (contains? #{"a" "b" "c" "d"} x))

(defn get-value [x registers]
  (if (is-register? x)
    (get registers x)
    (parse-int x)))

(defn execute-program [instructions registers]
  (let [instructions (vec instructions)]
    (loop [i 0 registers registers instructions instructions]
      (if (>= i (count instructions))
        registers
        (let [instr (instructions i)
              parts (str/split instr #"\s+")
              cmd (first parts)]
          (cond
            ;; Multiplication optimization pattern
            (and (< (+ i 5) (count instructions))
                 (= cmd "cpy")
                 (= (instructions (inc i)) "inc a")
                 (str/starts-with? (instructions (+ i 2)) "dec")
                 (str/starts-with? (instructions (+ i 3)) "jnz")
                 (= (instructions (+ i 4)) "dec d")
                 (= (instructions (+ i 5)) "jnz d -5"))
            (let [[_ x y] parts
                  _ (println "Optimizing multiplication pattern at" i)]
              (recur (+ i 6)
                     (-> registers
                         (update "a" + (* (get-value x registers) (registers "d")))
                         (assoc y 0)
                         (assoc "d" 0))
                     instructions))
            
            ;; Toggle instruction
            (= cmd "tgl")
            (let [x (second parts)
                  offset (get-value x registers)
                  target-idx (+ i offset)]
              (if (and (>= target-idx 0) (< target-idx (count instructions)))
                (let [target (instructions target-idx)
                      target-parts (str/split target #"\s+")
                      new-target (case (count target-parts)
                                   2 (if (= (first target-parts) "inc")
                                       (str "dec " (second target-parts))
                                       (str "inc " (second target-parts)))
                                   3 (if (= (first target-parts) "jnz")
                                       (str "cpy " (second target-parts) " " (nth target-parts 2))
                                       (str "jnz " (second target-parts) " " (nth target-parts 2)))
                                   target)]
                  (recur (inc i) registers (assoc instructions target-idx new-target)))
                (recur (inc i) registers instructions)))
            
            ;; Copy instruction
            (= cmd "cpy")
            (let [[_ x y] parts]
              (if (is-register? y)
                (recur (inc i) (assoc registers y (get-value x registers)) instructions)
                (recur (inc i) registers instructions)))
            
            ;; Increment instruction
            (= cmd "inc")
            (let [x (second parts)]
              (if (is-register? x)
                (recur (inc i) (update registers x inc) instructions)
                (recur (inc i) registers instructions)))
            
            ;; Decrement instruction
            (= cmd "dec")
            (let [x (second parts)]
              (if (is-register? x)
                (recur (inc i) (update registers x dec) instructions)
                (recur (inc i) registers instructions)))
            
            ;; Jump instruction
            (= cmd "jnz")
            (let [[_ x y] parts]
              (if (not= (get-value x registers) 0)
                (recur (+ i (get-value y registers)) registers instructions)
                (recur (inc i) registers instructions)))
            
            ;; Unknown instruction
            :else (recur (inc i) registers instructions)))))))

(defn -main []
  (let [instructions (read-input)
        registers {"a" 12 "b" 0 "c" 0 "d" 0}
        final-registers (execute-program instructions registers)]
    (println (final-registers "a"))))

(-main)
