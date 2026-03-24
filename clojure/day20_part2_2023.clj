
(ns solution
  (:require [clojure.string :as str]))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b]
  (if (or (zero? a) (zero? b))
    0
    (quot (abs (* (bigint a) (bigint b))) (gcd (bigint a) (bigint b)))))

(defn solve []
  (let [lines (str/split-lines (str/trim (slurp "input.txt")))
        parsed (into {} (for [line lines]
                          (let [[s d-str] (str/split line #" -> ")
                                dests (str/split d-str #", ")
                                [type name] (cond (= s "broadcaster") [:broadcaster s]
                                                  (str/starts-with? s "%") [:flip-flop (subs s 1)]
                                                  :else [:conjunction (subs s 1)])]
                            [name {:type type :dests dests :state false :mem {}}])))
        modules (reduce (fn [ms [name m]]
                          (reduce (fn [acc d]
                                    (if (= :conjunction (get-in acc [d :type]))
                                      (assoc-in acc [d :mem name] false)
                                      acc))
                                  ms (:dests m)))
                        parsed parsed)
        rx-feeder (first (for [[name m] modules :when (some #{"rx"} (:dests m))] name))
        feeder-inputs (into {} (for [[name m] modules :when (some #{rx-feeder} (:dests m))] [name nil]))]
    (loop [press-count 1
           modules modules
           cycles feeder-inputs]
      (let [[final-ms final-cyc]
            (loop [q (conj clojure.lang.PersistentQueue/EMPTY ["button" "broadcaster" false])
                   ms modules
                   cyc cycles]
              (if-let [[src dest pulse] (peek q)]
                (let [q (pop q)]
                  (if-let [m (get ms dest)]
                    (let [[out-pulse send? next-ms]
                          (case (:type m)
                            :broadcaster [pulse true ms]
                            :flip-flop (if (not pulse)
                                         [(not (:state m)) true (update ms dest assoc :state (not (:state m)))]
                                         [nil false ms])
                            :conjunction (let [nm (assoc-in ms [dest :mem src] pulse)
                                               all-h (every? true? (vals (get-in nm [dest :mem])))]
                                           [(not all-h) true nm]))
                          next-cyc (if (and out-pulse (contains? cyc dest) (nil? (get cyc dest)))
                                     (assoc cyc dest press-count)
                                     cyc)]
                      (if send?
                        (recur (reduce #(conj %1 [dest %2 out-pulse]) q (:dests m))
                               next-ms
                               next-cyc)
                        (recur q next-ms next-cyc)))
                    (recur q ms cyc)))
                [ms cyc]))]
        (if (every? some? (vals final-cyc))
          (println (reduce lcm (vals final-cyc)))
          (recur (inc press-count) final-ms final-cyc))))))

(solve)

