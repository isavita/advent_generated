
(ns packet-decoder
  (:require [clojure.string :as str]))

(def hex-to-bin
  {"0" "0000" "1" "0001" "2" "0010" "3" "0011"
   "4" "0100" "5" "0101" "6" "0110" "7" "0111"
   "8" "1000" "9" "1001" "A" "1010" "B" "1011"
   "C" "1100" "D" "1101" "E" "1110" "F" "1111"})

(defn hex-to-binary [hex-str]
  (->> hex-str
       (map str)
       (map hex-to-bin)
       (str/join "")))

(defn binary-to-long [binary-str]
  (Long/parseLong binary-str 2))

(defn parse-literal [binary-str]
  (loop [remaining binary-str
         value-str ""]
    (let [group (subs remaining 0 5)
          prefix (subs group 0 1)
          data (subs group 1 5)
          new-value-str (str value-str data)
          new-remaining (subs remaining 5)]
      (if (= prefix "0")
        {:value (binary-to-long new-value-str)
         :remaining new-remaining}
        (recur new-remaining new-value-str)))))

(defn parse-packet [binary-str]
  (let [version (binary-to-long (subs binary-str 0 3))
        type-id (binary-to-long (subs binary-str 3 6))]
    (if (= type-id 4)
      (let [{:keys [value remaining]} (parse-literal (subs binary-str 6))]
        {:version version
         :type-id type-id
         :value value
         :remaining remaining})
      (let [length-type-id (subs binary-str 6 7)]
        (if (= length-type-id "0")
          (let [total-length (binary-to-long (subs binary-str 7 22))
                sub-packets-str (subs binary-str 22 (+ 22 total-length))
                remaining (subs binary-str (+ 22 total-length))
                sub-packets (loop [remaining-sub sub-packets-str
                                   packets []]
                              (if (empty? remaining-sub)
                                packets
                                (let [parsed-packet (parse-packet remaining-sub)]
                                  (recur (:remaining parsed-packet)
                                         (conj packets parsed-packet)))))
                ]
            {:version version
             :type-id type-id
             :sub-packets sub-packets
             :remaining remaining})
          (let [num-sub-packets (binary-to-long (subs binary-str 7 18))
                remaining (subs binary-str 18)
                sub-packets (loop [remaining-sub remaining
                                   packets []
                                   count 0]
                              (if (or (empty? remaining-sub) (= count num-sub-packets))
                                packets
                                (let [parsed-packet (parse-packet remaining-sub)]
                                  (recur (:remaining parsed-packet)
                                         (conj packets parsed-packet)
                                         (inc count)))))
                ]
            {:version version
             :type-id type-id
             :sub-packets sub-packets
             :remaining (reduce (fn [acc p] (:remaining p)) remaining sub-packets)}))))))

(defn sum-versions [packet]
  (let [{:keys [version sub-packets]} packet]
    (if (empty? sub-packets)
      version
      (+ version (reduce + (map sum-versions sub-packets))))))

(defn calculate-value [packet]
  (let [{:keys [type-id value sub-packets]} packet]
    (case type-id
      0 (reduce + (map calculate-value sub-packets))
      1 (reduce * (map calculate-value sub-packets))
      2 (apply min (map calculate-value sub-packets))
      3 (apply max (map calculate-value sub-packets))
      4 value
      5 (if (> (calculate-value (first sub-packets)) (calculate-value (second sub-packets))) 1 0)
      6 (if (< (calculate-value (first sub-packets)) (calculate-value (second sub-packets))) 1 0)
      7 (if (= (calculate-value (first sub-packets)) (calculate-value (second sub-packets))) 1 0))))

(defn solve [input-str]
  (let [binary-str (hex-to-binary input-str)
        parsed-packet (parse-packet binary-str)]
    {:part1 (sum-versions parsed-packet)
     :part2 (calculate-value parsed-packet)}))

(defn -main []
  (let [input-str (slurp "input.txt")
        results (solve input-str)]
    (println "Part 1:" (:part1 results))
    (println "Part 2:" (:part2 results))))

(-main)
