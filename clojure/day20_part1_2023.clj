
(defn parse-module [line]
  (let [[module-str dest-str] (clojure.string/split line #" -> ")
        destinations (clojure.string/split dest-str #", ")
        prefix (first module-str)
        name (if (contains? #{\% \&} prefix) (subs module-str 1) module-str)]
    {:name name
     :prefix (if (contains? #{\% \&} prefix) prefix nil)
     :destinations destinations
     :state false
     :memory {}}))

(defn parse-input [input]
  (let [modules (reduce (fn [m line]
                          (let [module (parse-module line)]
                            (assoc m (:name module) module)))
                        {} input)
        modules (reduce (fn [m module]
                          (reduce (fn [m dest-name]
                                    (if-let [dest-module (get m dest-name)]
                                      (if (= (:prefix dest-module) \&)
                                        (assoc-in m [dest-name :memory (:name module)] :low)
                                        m)
                                      m))
                                  m (:destinations module)))
                        modules (vals modules))]
    modules))

(defn push-button [modules start-pulse num-cycles]
  (loop [cnt-low 0
         cnt-high 0
         pulse-queue [start-pulse]
         modules modules
         cycle-count 0]
    (if (= cycle-count num-cycles)
      [cnt-low cnt-high]
      (if (empty? pulse-queue)
        (recur cnt-low cnt-high [start-pulse] modules (inc cycle-count))
        (let [{:keys [value from-name to-name]} (first pulse-queue)
              pulse-queue (rest pulse-queue)
              [cnt-low cnt-high] (if (= value :low)
                                   [(inc cnt-low) cnt-high]
                                   [cnt-low (inc cnt-high)])]
          (if-not (contains? modules to-name)
            (recur cnt-low cnt-high pulse-queue modules cycle-count)
            (let [module (get modules to-name)
                  {:keys [prefix state memory destinations]} module
                  new-pulse-value (case prefix
                                    \% (if (= value :low)
                                         (let [new-state (not state)]
                                           (if new-state :high :low))
                                         nil)
                                    \& (let [updated-memory (assoc memory from-name value)
                                             is-high-for-all (every? #(= % :high) (vals updated-memory))]
                                         (if is-high-for-all :low :high))
                                    value)
                  modules (if (and prefix (= prefix \%) (= value :low))
                            (assoc-in modules [to-name :state] (not state))
                            (if (and prefix (= prefix \&))
                              (assoc-in modules [to-name :memory] (assoc memory from-name value))
                              modules))
                  new-pulses (if new-pulse-value
                               (map (fn [dest-name]
                                      {:value new-pulse-value
                                       :from-name to-name
                                       :to-name dest-name})
                                    destinations)
                               [])]
              (recur cnt-low cnt-high (concat pulse-queue new-pulses) modules cycle-count))))))))

(defn solve [input]
  (let [start-pulse {:value :low :from-name "button" :to-name "broadcaster"}
        num-cycles 1000
        modules (parse-input input)
        [cnt-low cnt-high] (push-button modules start-pulse num-cycles)]
    (* cnt-low cnt-high)))

(defn read-file [file-name]
  (clojure.string/split (slurp file-name) #"\n"))

(defn -main []
  (let [input (read-file "input.txt")]
    (println (solve input))))

(-main)
