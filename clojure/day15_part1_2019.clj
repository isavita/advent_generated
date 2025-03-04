
(ns intcode
  (:require [clojure.string :as str]))

(defn parse-program [file-path]
  (->> (slurp file-path)
       (str/trim)
       (str/split #",")
       (mapv #(Long/parseLong %))))

(defn get-param [memory ip mode relative-base offset]
  (let [param (get memory (+ ip offset) 0)
        address (case mode
                  0 (get memory param 0)
                  1 param
                  2 (+ relative-base (get memory param 0)))]
    (get memory address 0)))

(defn set-param [memory ip mode relative-base offset value]
  (let [param (get memory (+ ip offset) 0)
        address (case mode
                  0 param
                  2 (+ relative-base param))]
    (assoc memory address value)))

(defn run-intcode [memory]
  (let [memory (into {} (map-indexed vector memory))
        state (atom {:ip 0 :relative-base 0 :memory memory})]
    (loop []
      (let [{:keys [ip relative-base memory]} @state
            instruction (get memory ip 0)
            opcode (mod instruction 100)
            modes (mapv #(mod (quot instruction (int (Math/pow 10 (+ % 2)))) 10) [0 1 2])]
        (case opcode
          1 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                  param2 (get-param memory ip (nth modes 1) relative-base 2)
                  new-memory (set-param memory ip (nth modes 2) relative-base 3 (+ param1 param2))]
              (swap! state assoc :memory new-memory :ip (+ ip 4))
              (recur))
          2 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                  param2 (get-param memory ip (nth modes 1) relative-base 2)
                  new-memory (set-param memory ip (nth modes 2) relative-base 3 (* param1 param2))]
              (swap! state assoc :memory new-memory :ip (+ ip 4))
              (recur))
          3 (do
              (print "Input: ")
              (flush)
              (let [input-val (Long/parseLong (read-line))
                    new-memory (set-param memory ip (nth modes 0) relative-base 1 input-val)]
                (swap! state assoc :memory new-memory :ip (+ ip 2))
                (recur)))
          4 (let [output-val (get-param memory ip (nth modes 0) relative-base 1)]
              (println "Output:" output-val)
              (swap! state assoc :ip (+ ip 2))
              (recur))
          5 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                  param2 (get-param memory ip (nth modes 1) relative-base 2)]
              (if (not= param1 0)
                (swap! state assoc :ip param2)
                (swap! state assoc :ip (+ ip 3)))
              (recur))
          6 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                  param2 (get-param memory ip (nth modes 1) relative-base 2)]
              (if (= param1 0)
                (swap! state assoc :ip param2)
                (swap! state assoc :ip (+ ip 3)))
              (recur))
          7 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                  param2 (get-param memory ip (nth modes 1) relative-base 2)
                  new-memory (set-param memory ip (nth modes 2) relative-base 3 (if (< param1 param2) 1 0))]
              (swap! state assoc :memory new-memory :ip (+ ip 4))
              (recur))
          8 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                  param2 (get-param memory ip (nth modes 1) relative-base 2)
                  new-memory (set-param memory ip (nth modes 2) relative-base 3 (if (= param1 param2) 1 0))]
              (swap! state assoc :memory new-memory :ip (+ ip 4))
              (recur))
          9 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)]
              (swap! state update :relative-base + param1)
              (swap! state assoc :ip (+ ip 2))
              (recur))
          99 (println "Halted")
          (println "Unknown opcode:" opcode))))))

(defn run-program [program]
      (run-intcode program)
      )
      
(defn get-opposite-direction [direction]
    ({1 2, 2 1, 3 4, 4 3} direction))

(defn send-move-command [computer direction]
  (let [output (atom nil)
        input-fn (fn [] direction)
        output-fn (fn [val] (reset! output val))
        memory (:memory @computer)
        ip (:ip @computer)
        relative-base (:relative-base @computer)
       
        state (atom {:ip ip, :relative-base relative-base, :memory memory, :input-fn input-fn, :output-fn output-fn})
        
        _ (loop []
            (let [{:keys [ip relative-base memory input-fn output-fn]} @state
                 instruction (get memory ip 0)
                 opcode (mod instruction 100)
                 modes (mapv #(mod (quot instruction (int (Math/pow 10 (+ % 2)))) 10) [0 1 2])]
             (case opcode
               1 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                       param2 (get-param memory ip (nth modes 1) relative-base 2)
                       new-memory (set-param memory ip (nth modes 2) relative-base 3 (+ param1 param2))]
                   (swap! state assoc :memory new-memory :ip (+ ip 4))
                   (recur))
               2 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                       param2 (get-param memory ip (nth modes 1) relative-base 2)
                       new-memory (set-param memory ip (nth modes 2) relative-base 3 (* param1 param2))]
                   (swap! state assoc :memory new-memory :ip (+ ip 4))
                   (recur))
               3 (let [new-memory (set-param memory ip (nth modes 0) relative-base 1 (input-fn))]
                     (swap! state assoc :memory new-memory :ip (+ ip 2))
                     (recur))
               4 (let [output-val (get-param memory ip (nth modes 0) relative-base 1)]
                         (output-fn output-val)
                         (swap! state assoc :ip (+ ip 2))
                         )
               5 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                       param2 (get-param memory ip (nth modes 1) relative-base 2)]
                   (if (not= param1 0)
                     (swap! state assoc :ip param2)
                     (swap! state assoc :ip (+ ip 3)))
                   (recur))
               6 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                       param2 (get-param memory ip (nth modes 1) relative-base 2)]
                   (if (= param1 0)
                     (swap! state assoc :ip param2)
                     (swap! state assoc :ip (+ ip 3)))
                   (recur))
               7 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                       param2 (get-param memory ip (nth modes 1) relative-base 2)
                       new-memory (set-param memory ip (nth modes 2) relative-base 3 (if (< param1 param2) 1 0))]
                   (swap! state assoc :memory new-memory :ip (+ ip 4))
                   (recur))
               8 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)
                       param2 (get-param memory ip (nth modes 1) relative-base 2)
                       new-memory (set-param memory ip (nth modes 2) relative-base 3 (if (= param1 param2) 1 0))]
                   (swap! state assoc :memory new-memory :ip (+ ip 4))
                   (recur))
               9 (let [param1 (get-param memory ip (nth modes 0) relative-base 1)]
                   (swap! state update :relative-base + param1)
                   (swap! state assoc :ip (+ ip 2))
                   (recur))
               99 (reduced -1)
              
               )))]
    
    (if (nil? @output)
        -1
        @output)
   ))
        
(defn explore [program]
  (let [direction-map {1 [0 -1], 2 [0 1], 3 [-1 0], 4 [1 0]}
        grid (atom {(vector 0 0) 1})
        queue (clojure.lang.PersistentQueue/EMPTY)
        queue (conj queue [[0 0] 0])
        visited (atom #{[0 0]})
        computer (atom {:ip 0, :relative-base 0, :memory (into {} (map-indexed vector program))})]
        
    (loop [q queue]
      (if (empty? q)
        -1
        (let [[[x y] steps] (peek q)
              q (pop q)]
          (doseq [direction [1 2 3 4]]
            (let [[dx dy] (direction-map direction)
                  new-pos [(+ x dx) (+ y dy)]]
              (if (not (contains? @visited new-pos))
                (let [status (send-move-command computer direction)]
                  (cond
                    (= status 0) (swap! grid assoc new-pos 0)
                    (or (= status 1) (= status 2)) (do
                                                    (swap! grid assoc new-pos (if (= status 1) 1 2))
                                                    (swap! visited conj new-pos)
                                                    (when (= status 2)
                                                        (println "Fewest number of movement commands to reach the oxygen system:" (+ steps 1)))
                                                    )
                    :else (println "Unexpected status" status)
                    )
                    (when (and (not= status 0) (not= status -1))
                      (send-move-command computer (get-opposite-direction direction)))
                   (if (= status 2)
                    (+ steps 1)
                    (recur (conj q [new-pos (+ steps 1)])))
                    ) 
                )
                (recur q)
               )))))))

(defn -main []
  (let [program (parse-program "input.txt")]
     (explore program)
     ))
