(require '[clojure.string :as str])

(defn abba? [s]
  (some #(and (= (first %) (nth % 3))
              (= (second %) (nth % 2))
              (not= (first %) (second %)))
        (partition 4 1 s)))

(defn aba-patterns [s]
  (filter #(and (= (first %) (nth % 2))
                (not= (first %) (second %)))
          (partition 3 1 s)))

(defn supports-tls? [[supernets hypernets]]
  (and (some abba? supernets)
       (not-any? abba? hypernets)))

(defn supports-ssl? [[supernets hypernets]]
  (let [abas (mapcat aba-patterns supernets)
        babs (map (fn [[a b _]] [b a b]) abas)]
    (some #(some (fn [bab] (some (partial str/includes? %) bab)) hypernets) babs)))

(defn parse-ip [s]
  (let [parts (str/split s #"[\[\]]")]
    [(take-nth 2 parts) (take-nth 2 (rest parts))])) ; [supernets hypernets]

(defn solve-puzzle [input-file]
  (let [ips (map parse-ip (str/split-lines (slurp input-file)))
        tls-count (count (filter supports-tls? ips))
        ssl-count (count (filter supports-ssl? ips))]
    (println "Part 1:" tls-count)
    (println "Part 2:" ssl-count)))

(solve-puzzle "input.txt")
