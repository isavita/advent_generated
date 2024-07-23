
(ns registers
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-instruction [line]
  (let [[reg op amount _ cond-reg cond-op cond-val] (str/split line #"\s+")]
    {:reg reg
     :op op
     :amount (Integer. amount)
     :cond-reg cond-reg
     :cond-op cond-op
     :cond-val (Integer. cond-val)}))

(defn evaluate-condition [registers cond-reg cond-op cond-val]
  (let [reg-val (get registers cond-reg 0)]
    (case cond-op
      ">"  (> reg-val cond-val)
      "<"  (< reg-val cond-val)
      ">=" (>= reg-val cond-val)
      "<=" (<= reg-val cond-val)
      "==" (== reg-val cond-val)
      "!=" (not= reg-val cond-val))))

(defn update-register [registers reg op amount]
  (let [current-value (get registers reg 0)
        new-value (case op
                    "inc" (+ current-value amount)
                    "dec" (- current-value amount))]
    (assoc registers reg new-value)))

(defn process-instructions [instructions]
  (let [initial-registers {}]
    (reduce (fn [{:keys [registers max-ever]} instruction]
              (if (evaluate-condition registers (:cond-reg instruction) (:cond-op instruction) (:cond-val instruction))
                (let [updated-registers (update-register registers (:reg instruction) (:op instruction) (:amount instruction))
                      current-max (apply max (vals updated-registers))]
                  {:registers updated-registers
                   :max-ever (max max-ever current-max)})
                {:registers registers
                 :max-ever max-ever}))
            {:registers initial-registers :max-ever Integer/MIN_VALUE}
            instructions)))

(defn main []
  (let [instructions (->> (slurp "input.txt")
                          (str/split-lines)
                          (map parse-instruction))
        {:keys [registers max-ever]} (process-instructions instructions)]
    (println "Largest value in any register after processing:" (apply max (vals registers)))
    (println "Highest value held in any register during processing:" max-ever)))

(main)
