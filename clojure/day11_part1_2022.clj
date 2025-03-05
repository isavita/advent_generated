
(ns monkey-business
  (:require [clojure.string :as str]))

(defn parse-int [s]
  (Long/parseLong s))

(defn parse-items [line]
  (mapv parse-int (re-seq #"\d+" line)))

(defn parse-operation [line]
  (let [[_ op val] (re-find #"new = old (.) (.+)" line)]
    (case op
      "*" (if (= val "old")
            (fn [old] (* old old))
            (fn [old] (* old (parse-int val))))
      "+" (fn [old] (+ old (parse-int val))))))

(defn parse-test [lines]
  (let [divisor (parse-int (re-find #"\d+" (nth lines 0)))
        true-target (parse-int (re-find #"\d+" (nth lines 1)))
        false-target (parse-int (re-find #"\d+" (nth lines 2)))]
    {:divisor divisor
     :true-target true-target
     :false-target false-target}))

(defn parse-monkey [monkey-str]
  (let [lines (str/split-lines monkey-str)]
    {:items (parse-items (nth lines 1))
     :operation (parse-operation (nth lines 2))
     :test (parse-test (drop 3 lines))
     :inspections (atom 0)}))

(defn parse-monkeys [input]
  (->> (str/split input #"\n\n")
       (map parse-monkey)
       (vec)))

(defn process-item [monkeys monkey-idx item]
  (let [monkey (nth monkeys monkey-idx)
        {:keys [operation test inspections]} monkey
        {:keys [divisor true-target false-target]} test
        new-worry-level (quot (operation item) 3)
        target-monkey (if (zero? (mod new-worry-level divisor))
                        true-target
                        false-target)]
    (swap! inspections inc)
    (update-in monkeys [target-monkey :items] conj new-worry-level)))


(defn do-round [monkeys]
  (reduce
   (fn [ms i]
     (let [{:keys [items]} (nth ms i)]
       (reduce (fn [updated-ms item]
                 (process-item updated-ms i item))
               (assoc-in ms [i :items] []) ; Clear current monkey's items.
               items)))
   monkeys
   (range (count monkeys))))


(defn solve [input rounds]
  (let [monkeys (parse-monkeys input)]
    (->> (iterate do-round monkeys)
         (drop rounds)
         (first)
         (map (fn [m] @(:inspections m)))
         (sort >)
         (take 2)
         (reduce *))))

(defn -main []
  (let [input (slurp "input.txt")
        result (solve input 20)]
    (println result)))

(-main)
