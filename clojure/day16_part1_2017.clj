
(defn spin [programs x]
  (let [n (count programs)]
    (vec (map (fn [i] (nth programs (mod (- i x) n))) (range n)))))

(defn exchange [programs a b]
  (let [temp (nth programs a)]
    (-> programs
        (assoc a (nth programs b))
        (assoc b temp))))

(defn partner [programs a b]
  (let [index-a (.indexOf programs a)
        index-b (.indexOf programs b)]
    (exchange programs index-a index-b)))

(defn solve [moves programs]
  (reduce
   (fn [progs move]
     (case (first move)
       \s (spin progs (Integer/parseInt (subs move 1)))
       \x (let [[a b] (map #(Integer/parseInt %) (clojure.string/split (subs move 1) #"/"))]
            (exchange progs a b))
       \p (let [[a b] (map first (clojure.string/split (subs move 1) #"/"))]
            (partner progs a b))
       progs))
   programs
   moves))

(defn main []
  (let [moves (clojure.string/split (slurp "input.txt") #",")
        programs (vec "abcdefghijklmnop")]
    (println (apply str (solve moves programs)))))

(main)
