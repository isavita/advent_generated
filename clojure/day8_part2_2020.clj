(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-instruction [line]
  (let [[op arg] (str/split line #" ")]
    {:op op :arg (Integer/parseInt arg)}))

(defn execute [instructions]
  (loop [acc 0
         pc 0
         visited #{}]
    (if (contains? visited pc)
      [false acc]
      (if (>= pc (count instructions))
        [true acc]
        (let [instruction (nth instructions pc)
              visited (conj visited pc)]
          (case (:op instruction)
            "acc" (recur (+ acc (:arg instruction)) (inc pc) visited)
            "jmp" (recur acc (+ pc (:arg instruction)) visited)
            "nop" (recur acc (inc pc) visited)))))))

(defn fix-program [instructions]
  (for [i (range (count instructions))
        :let [instruction (nth instructions i)]
        :when (or (= (:op instruction) "jmp") (= (:op instruction) "nop"))
        :let [fixed-instructions (assoc instructions i (assoc instruction :op (if (= (:op instruction) "jmp") "nop" "jmp")))]
        :let [[terminated acc] (execute fixed-instructions)]
        :when terminated]
    acc))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [instructions (vec (map parse-instruction (line-seq rdr)))]
      (println (first (fix-program instructions))))))

(-main)