(ns day7
  (:require [clojure.string :as str]))

(defn parse-command [line]
  (let [[_ cmd arg] (re-find #"\$ (\w+)\s*(.*)" line)]
    {:type :command, :cmd cmd, :arg arg}))

(defn parse-output [line]
  (if-let [[_ size name] (re-find #"(\d+) (.*)" line)]
    {:type :file, :size (Integer/parseInt size), :name name}
    {:type :dir, :name (second (str/split line #" "))}))

(defn parse-line [line]
  (if (str/starts-with? line "$")
    (parse-command line)
    (parse-output line)))

(defn build-fs [lines]
  (loop [remaining lines
         path []
         fs {}]
    (if (empty? remaining)
      fs
      (let [item (parse-line (first remaining))]
        (case (:type item)
          :command (case (:cmd item)
                     "cd" (case (:arg item)
                            "/" (recur (rest remaining) [] fs)
                            ".." (recur (rest remaining) (pop path) fs)
                            (recur (rest remaining) (conj path (:arg item)) fs))
                     "ls" (recur (rest remaining) path fs))
          :file (recur (rest remaining) path (assoc-in fs (conj path (:name item)) (:size item)))
          :dir (recur (rest remaining) path fs))))))

(defn dir-sizes [fs]
  (letfn [(size [item]
            (if (number? item)
              item
              (reduce + (map size (vals item)))))]
    (loop [to-visit (list [[] fs])
           sizes {}]
      (if (empty? to-visit)
        sizes
        (let [[path dir] (first to-visit)
              dir-size (size dir)]
          (recur (into (rest to-visit)
                       (for [[name subdir] dir
                             :when (map? subdir)]
                         [(conj path name) subdir]))
                 (assoc sizes path dir-size)))))))

(defn solve [input]
  (let [fs (build-fs (str/split-lines input))
        sizes (dir-sizes fs)
        total-size (get sizes [])
        unused-space (- 70000000 total-size)
        required-space (- 30000000 unused-space)]
    {:part1 (reduce + (filter #(<= % 100000) (vals sizes)))
     :part2 (apply min (filter #(>= % required-space) (vals sizes)))}))

(defn -main [& args]
  (let [input (slurp (or (first args) "input.txt"))
        result (solve input)]
    (println "Part 1:" (:part1 result))
    (println "Part 2:" (:part2 result))))
