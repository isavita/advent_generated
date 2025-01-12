
(import java.security.MessageDigest)

(defn md5-hash [input]
  (let [digest (MessageDigest/getInstance "MD5")]
    (.update digest (.getBytes input))
    (->> (.digest digest)
         (map (fn [b] (format "%02x" (bit-and b 0xff))))
         (apply str))))

(defn find-triplet [hash]
  (->> (partition 3 1 hash)
       (filter (fn [[a b c]] (= a b c)))
       (map first)
       first
       str))

(defn solve [salt]
  (loop [index 0
         keys 0
         cache {}]
    (if (>= keys 64)
      (dec index)
      (let [hash (or (get cache index) (md5-hash (str salt index)))
            triplet (find-triplet hash)]
        (if (not (empty? triplet))
          (let [found (some (fn [i]
                              (let [next-hash (or (get cache (+ index i)) (md5-hash (str salt (+ index i))))]
                                (when (clojure.string/includes? next-hash (apply str (repeat 5 triplet)))
                                  true)))
                            (range 1 1001))]
            (recur (inc index) (if found (inc keys) keys) (assoc cache index hash)))
          (recur (inc index) keys (assoc cache index hash)))))))

(let [salt (slurp "input.txt")]
  (println (solve (clojure.string/trim salt))))
