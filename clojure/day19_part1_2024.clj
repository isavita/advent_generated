
(ns day19.core
  (:require [clojure.string :as str]))

(defn solve [patterns designs]
  (let [patterns-set (set patterns)]
    (count
     (filter
      (fn [design]
        (loop [remaining design
               dp (vec (repeat (count design) false))] ; Dynamic programming table
          (if (empty? remaining)
            true ; Design is fully matched
            (let [len (count remaining)]
              (if (and (>= len 1) (not (dp (- (count design) len))))
                (let [matched (reduce (fn [acc i]
                                        (let [sub (subs remaining 0 i)]
                                          (if (contains? patterns-set sub)
                                            (if (= i len)
                                              (reduced true)   ; Entire remaining string matched
                                              (if (recur (subs remaining i)
                                                         (assoc dp (- (count design) len) true))                                                     
                                                (reduced true)
                                                acc))
                                            acc)))
                                      false
                                      (range 1 (inc len)))]
                  matched)
                false)))))
      designs))))

(defn -main [& _args]
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [lines (line-seq rdr)
          [patterns-str designs-str] (str/split (str/join "\n" lines) #"\n\n")
          patterns (str/split patterns-str #", ")
          designs (str/split-lines designs-str)]
      (println (solve patterns designs)))))
