
(defn solve [instructions]
  (loop [registers {"a" 0 "b" 0} pc 0]
    (if (>= pc (count instructions))
      (get registers "b")
      (let [parts (clojure.string/split (nth instructions pc) #" ")
            instruction (first parts)]
        (case instruction
          "hlf" (recur (assoc registers (second parts) (quot (get registers (second parts)) 2)) (inc pc))
          "tpl" (recur (assoc registers (second parts) (* 3 (get registers (second parts)))) (inc pc))
          "inc" (recur (assoc registers (second parts) (inc (get registers (second parts)))) (inc pc))
          "jmp" (recur registers (+ pc (Integer/parseInt (second parts))))
          "jie" (if (even? (get registers (subs (second parts) 0 1)))
                  (recur registers (+ pc (Integer/parseInt (nth parts 2))))
                  (recur registers (inc pc)))
          "jio" (if (= 1 (get registers (subs (second parts) 0 1)))
                  (recur registers (+ pc (Integer/parseInt (nth parts 2))))
                  (recur registers (inc pc))))))))

(defn main []
  (let [instructions (clojure.string/split (slurp "input.txt") #"\n")]
    (println (solve instructions))))

(main)
