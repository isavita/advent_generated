
(require '[clojure.string :as str])

(defn parse-rule [rule-str]
  (if (str/includes? rule-str ":")
    (let [[cond-str wf] (str/split rule-str #":")
          category (subs cond-str 0 1)
          operator (subs cond-str 1 2)
          num (Integer/parseInt (subs cond-str 2))]
      {:category category :operator operator :num num :workflow wf})
    {:workflow rule-str}))

(defn parse-workflow [line]
  (let [[name rules-str] (str/split line #"\{")
        rules-str (subs rules-str 0 (dec (count rules-str)))
        rules (mapv parse-rule (str/split rules-str #","))]
    [name rules]))

(defn parse-input [input]
  (let [[workflows-str parts-str] (str/split input #"\n\n")
        workflows (into {} (map parse-workflow (str/split-lines workflows-str)))
        parts (map (fn [part-str]
                     (let [[_ x m a s] (re-matches #"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}" part-str)]
                       {:x (Integer/parseInt x)
                        :m (Integer/parseInt m)
                        :a (Integer/parseInt a)
                        :s (Integer/parseInt s)}))
                   (str/split-lines parts-str))]
    [workflows parts]))

(defn apply-rule [rule part]
  (if (:category rule)
    (let [value (get part (keyword (:category rule)))]
      (case (:operator rule)
        ">" (> value (:num rule))
        "<" (< value (:num rule))))
    true))

(defn process-part [workflows part wf-name]
  (cond
    (= wf-name "A") (apply + (vals part))
    (= wf-name "R") 0
    :else (let [rules (get workflows wf-name)]
            (loop [rules rules]
              (if (empty? rules)
                0
                (let [rule (first rules)]
                  (if (apply-rule rule part)
                    (process-part workflows part (:workflow rule))
                    (recur (rest rules)))))))))

(defn part1 [workflows parts]
  (apply + (map #(process-part workflows % "in") parts)))

(defn split-interval [interval op num]
  (let [[start end] interval]
    (case op
      ">" (if (> num end)
            [[nil nil] [start end]]
            (if (>= num start)
              [[(inc num) end] [start num]]
              [[start end] [nil nil]]))
      "<" (if (< num start)
            [[nil nil] [start end]]
            (if (<= num end)
              [[start (dec num)] [num end]]
              [[start end] [nil nil]])))))

(defn count-combinations [intervals wf-name workflows]
  (cond
    (= wf-name "A") (apply * (map (fn [[s e]] (if (and s e) (- (inc e) s) 0)) (vals intervals)))
    (= wf-name "R") 0
    :else (let [rules (get workflows wf-name)]
            (loop [rules rules
                   intervals intervals
                   total 0]
              (if (empty? rules)
                total
                (let [rule (first rules)]
                  (if (:category rule)
                    (let [[valid-int invalid-int] (split-interval (get intervals (keyword (:category rule))) (:operator rule) (:num rule))
                          new-intervals (if (some nil? valid-int)
                                          intervals
                                          (assoc intervals (keyword (:category rule)) valid-int))]
                      (recur (rest rules)
                             (if (some nil? invalid-int)
                               intervals
                               (assoc intervals (keyword (:category rule)) invalid-int))
                             (+ total (if (some nil? valid-int)
                                        0
                                        (count-combinations new-intervals (:workflow rule) workflows)))))
                    (recur (rest rules)
                           intervals
                           (+ total (count-combinations intervals (:workflow rule) workflows))))))))))

(defn part2 [workflows]
  (count-combinations {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]} "in" workflows))

(defn -main []
  (let [input (slurp "input.txt")
        [workflows parts] (parse-input input)]
    (println (part2 workflows))))

(-main)
