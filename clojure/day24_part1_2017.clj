(ns day24
  (:gen-class))

(defn parse-input [input]
  (map #(let [[_ a b] (re-matches #"(\d+)/(\d+)" %)]
           [(Integer/parseInt a) (Integer/parseInt b)])
       (clojure.string/split-lines input)))

(defn connect [components port]
  (filter #(or (= (first %) port) (= (second %) port)) components))

(defn build-bridge [components port strength]
  (let [next-components (connect components port)]
    (if (empty? next-components)
      strength
      (apply max (map #(build-bridge (remove #{%} components) (if (= port (first %)) (second %) (first %)) (+ strength (apply + %))) next-components)))))

(defn -main []
  (let [input (slurp "input.txt")
        components (parse-input input)]
    (println (build-bridge components 0 0))))

(-main)