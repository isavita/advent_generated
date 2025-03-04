
(defn parse-rule [rule]
  (let [[bag-color contents] (clojure.string/split rule #" bags contain ")
        bags (if (= contents "no other bags.")
               []
               (clojure.string/split contents #", "))]
    [bag-color (map (fn [bag] (clojure.string/join " " (subvec (clojure.string/split bag #" ") 1 3))) bags)]))

(defn build-contain-dict [rules]
  (into {} (map parse-rule rules)))

(defn check-bag [contain-dict bag-color]
  (if (= bag-color "shiny gold")
    true
    (if-not (contain-dict bag-color)
      false
      (some #(check-bag contain-dict %) (contain-dict bag-color)))))

(defn solve [rules]
  (let [contain-dict (build-contain-dict rules)]
    (dec (count (filter #(check-bag contain-dict %) (keys contain-dict))))))

(defn main []
  (let [rules (clojure.string/split (slurp "input.txt") #"\n")]
    (println (solve rules))))

(main)
