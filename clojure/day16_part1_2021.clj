
(defn hex->bin [hex]
  (->> hex
       (map #(Integer/parseInt (str %) 16))
       (map #(format "%4s" (Integer/toBinaryString %)))
       (map #(clojure.string/replace % " " "0"))
       (apply str)))

(defn parse-packet [bin-str idx]
  (let [version (-> (subs bin-str idx (+ idx 3)) (Integer/parseInt 2))
        type-id (-> (subs bin-str (+ idx 3) (+ idx 6)) (Integer/parseInt 2))
        idx (+ idx 6)]
    (if (= type-id 4)
      (loop [i idx]
        (if (= (nth bin-str i) \1)
          (recur (+ i 5))
          [(+ version 0) (+ i 5)]))
      (let [length-type-id (-> (subs bin-str idx (+ idx 1)) (Integer/parseInt 2))
            idx (inc idx)]
        (if (= length-type-id 0)
          (let [sub-packet-length (-> (subs bin-str idx (+ idx 15)) (Integer/parseInt 2))
                idx (+ idx 15)]
            (loop [version-sum version
                   current-idx idx
                   remaining-length sub-packet-length]
              (if (zero? remaining-length)
                [version-sum current-idx]
                (let [[sub-version new-idx] (parse-packet bin-str current-idx)]
                  (recur (+ version-sum sub-version)
                         new-idx
                         (- remaining-length (- new-idx current-idx)))))))
          (let [num-sub-packets (-> (subs bin-str idx (+ idx 11)) (Integer/parseInt 2))
                idx (+ idx 11)]
            (loop [version-sum version
                   current-idx idx
                   remaining-packets num-sub-packets]
              (if (zero? remaining-packets)
                [version-sum current-idx]
                (let [[sub-version new-idx] (parse-packet bin-str current-idx)]
                  (recur (+ version-sum sub-version)
                         new-idx
                         (dec remaining-packets)))))))))))

(defn solve []
  (let [hex-str (-> (slurp "input.txt") clojure.string/trim)
        bin-str (hex->bin hex-str)]
    (-> (parse-packet bin-str 0) first)))

(println (solve))
