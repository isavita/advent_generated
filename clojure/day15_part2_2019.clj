
(require '[clojure.string :as str])

(defn parse-input [f]
  (let [s (str/trim (slurp f))
        nums (mapv #(Long/parseLong %) (str/split s #","))]
    (zipmap (range) nums)))

(defn get-mem [mem addr] (get mem addr 0))

(defn get-params [mem ip rb modes n]
  (mapv (fn [i mode]
          (let [v (get-mem mem (+ ip i 1))]
            (case mode
              0 (get-mem mem v)
              1 v
              2 (get-mem mem (+ rb v)))))
        (range n) modes))

(defn get-addr [mem ip rb mode offset]
  (let [v (get-mem mem (+ ip offset))]
    (if (= mode 2) (+ rb v) v)))

(defn run [state]
  (let [{:keys [mem ip rb in out]} state
        inst (get-mem mem ip)
        opcode (mod inst 100)
        modes (map #(mod (quot inst %) 10) [100 1000 10000])]
    (case opcode
      1 (let [p (get-params mem ip rb modes 2)
              a (get-addr mem ip rb (nth modes 2) 3)]
          (recur (assoc state :mem (assoc mem a (apply + p)) :ip (+ ip 4))))
      2 (let [p (get-params mem ip rb modes 2)
              a (get-addr mem ip rb (nth modes 2) 3)]
          (recur (assoc state :mem (assoc mem a (apply * p)) :ip (+ ip 4))))
      3 (if (empty? in) state
            (let [a (get-addr mem ip rb (first modes) 1)]
              (recur (assoc state :mem (assoc mem a (first in)) :in (rest in) :ip (+ ip 2)))))
      4 (let [p (get-params mem ip rb modes 1)]
          (assoc state :out (conj out (first p)) :ip (+ ip 2)))
      5 (let [p (get-params mem ip rb modes 2)]
          (recur (assoc state :ip (if (not= 0 (first p)) (second p) (+ ip 3)))))
      6 (let [p (get-params mem ip rb modes 2)]
          (recur (assoc state :ip (if (= 0 (first p)) (second p) (+ ip 3)))))
      7 (let [p (get-params mem ip rb modes 2)
              a (get-addr mem ip rb (nth modes 2) 3)]
          (recur (assoc state :mem (assoc mem a (if (< (first p) (second p)) 1 0)) :ip (+ ip 4))))
      8 (let [p (get-params mem ip rb modes 2)
              a (get-addr mem ip rb (nth modes 2) 3)]
          (recur (assoc state :mem (assoc mem a (if (= (first p) (second p)) 1 0)) :ip (+ ip 4))))
      9 (let [p (get-params mem ip rb modes 1)]
          (recur (assoc state :rb (+ rb (first p)) :ip (+ ip 2))))
      99 (assoc state :halted true))))

(def dirs {1 [0 1] 2 [0 -1] 3 [-1 0] 4 [1 0]})

(defn explore [comp pos grid]
  (reduce (fn [g [d move]]
            (let [np (mapv + pos move)]
              (if (contains? g np) g
                  (let [res (run (assoc comp :in [d] :out []))
                        status (first (:out res))]
                    (case status
                      0 (assoc g np :wall)
                      1 (explore res np (assoc g np :path))
                      2 (explore res np (assoc g np :oxy)))))))
          grid dirs))

(defn bfs [grid start target-type]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [start 0])
         v {start 0}]
    (if (empty? q) v
        (let [[p d] (peek q)
              nq (pop q)]
          (if (and target-type (= (grid p) target-type)) d
              (let [neighbors (for [m (vals dirs)
                                    :let [np (mapv + p m)]
                                    :when (and (not (contains? v np))
                                               (contains? #{:path :oxy} (grid np)))]
                                np)]
                (recur (reduce #(conj %1 [%2 (inc d)]) nq neighbors)
                       (reduce #(assoc %1 %2 (inc d)) v neighbors))))))))

(let [mem (parse-input "input.txt")
      init-comp {:mem mem :ip 0 :rb 0 :in [] :out []}
      full-grid (explore init-comp [0 0] {[0 0] :path})
      oxy-pos (first (first (filter #(= (val %) :oxy) full-grid)))
      dist-map (bfs full-grid [0 0] nil)]
  (println (dist-map oxy-pos))
  (println (apply max (vals (bfs full-grid oxy-pos nil)))))
