(ns monkey-math
  (:gen-class))

(defn parse-monkey [line]
  (let [[_ name job] (re-matches #"(\w+): (.+)" line)]
    [(keyword name) (if (re-matches #"\d+" job)
                      (Long/parseLong job)
                      (let [[_ a op b] (re-matches #"(\w+) ([+-/*]) (\w+)" job)]
                        [(keyword a) (keyword op) (keyword b)]))]))

(defn parse-input [input]
  (into {} (map parse-monkey input)))

(defn calculate [monkeys name]
  (let [job (get monkeys name)]
    (if (number? job)
      job
      (let [[a op b] job]
        (case op
          :+ (+ (calculate monkeys a) (calculate monkeys b))
          :- (- (calculate monkeys a) (calculate monkeys b))
          :* (* (calculate monkeys a) (calculate monkeys b))
          :/ (quot (calculate monkeys a) (calculate monkeys b)))))))

(defn -main []
  (let [input (clojure.string/split (slurp "input.txt") #"\n")
        monkeys (parse-input input)]
    (println (calculate monkeys :root))))

(-main)