
(require '[clojure.java.io :as io])

(defn parse-int [s] (Long/parseLong s))

(defn upd-dirs [sizes path size]
  (reduce (fn [acc p] (update acc p + size)) sizes path))

(defn walk-dirs []
  (with-open [rdr (io/reader "input.txt")]
    (loop [lines (line-seq rdr)
           sizes {"/" 0}
           stack ["/"]]
      (if-let [line (first lines)]
        (cond
          (.startsWith line "$ cd")
          (let [dir (subs line 5)]
            (cond
              (= dir "/") (recur (rest lines) sizes ["/"])
              (= dir "..") (recur (rest lines) sizes (pop stack))
              :else (let [new-p (str (peek stack) dir "/")]
                      (recur (rest lines) (assoc sizes new-p 0) (conj stack new-p)))))
          (Character/isDigit (first line))
          (let [size (parse-int (first (clojure.string/split line #" ")))]
            (recur (rest lines) (upd-dirs sizes stack size) stack))
          :else (recur (rest lines) sizes stack))
        sizes))))

(->> (walk-dirs)
     vals
     (filter #(<= % 100000))
     (reduce +)
     println)
