
(import '[java.security MessageDigest])

(defn md5-hash [input]
  (let [digest (.digest (MessageDigest/getInstance "MD5") (.getBytes input))]
    (apply str (map (fn [b] (format "%02x" (bit-and b 0xff))) digest))))

(defn solve []
  (let [door-id (-> "input.txt" slurp clojure.string/trim)
        password (atom (vec (repeat 8 nil)))
        found (atom 0)]
    (loop [i 0]
      (when (< @found 8)
        (let [hash (md5-hash (str door-id i))]
          (when (clojure.string/starts-with? hash "00000")
            (let [pos (nth hash 5)
                  val (nth hash 6)]
              (when (and (<= (int \0) (int pos) (int \7))
                         (nil? (get @password (- (int pos) (int \0)))))
                (swap! password assoc (- (int pos) (int \0)) val)
                (swap! found inc)))))
        (recur (inc i))))
    (apply str @password)))

(println (solve))
