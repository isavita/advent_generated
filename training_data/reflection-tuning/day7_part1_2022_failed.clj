(ns day7
  (:require [clojure.string :as str]))

(defn parse-command [line]
  (let [[_ cmd arg] (re-find #"\$ (\w+)\s*(.*)" line)]
    (case cmd
      "cd" [:cd arg]
      "ls" [:ls]
      nil)))

(defn parse-ls-output [line]
  (if-let [[_ size name] (re-find #"(\d+) (.*)" line)]
    [:file (Integer/parseInt size) name]
    (when-let [[_ name] (re-find #"dir (.*)" line)]
      [:dir name])))

(defn build-fs [lines]
  (loop [lines lines
         path []
         fs {}]
    (if-let [line (first lines)]
      (if-let [cmd (parse-command line)]
        (case (first cmd)
          :cd (recur (rest lines)
                     (case (second cmd)
                       "/" ["/"]
                       ".." (pop path)
                       (conj path (second cmd)))
                     fs)
          :ls (recur (rest lines) path fs))
        (let [entry (parse-ls-output line)]
          (if entry
            (recur (rest lines)
                   path
                   (update-in fs path (fnil conj []) entry))
            (recur (rest lines) path fs))))
      fs)))

(defn dir-size [dir]
  (if (nil? dir)
    0
    (reduce + (map (fn [[type & rest]]
                     (case type
                       :file (first rest)
                       :dir (dir-size (second rest))))
                   dir))))

(defn find-small-dirs [fs max-size]
  (let [sizes (atom [])]
    (letfn [(traverse [dir]
              (when dir
                (let [size (dir-size dir)]
                  (when (<= size max-size)
                    (swap! sizes conj size))
                  (doseq [[type & rest] dir]
                    (when (= type :dir)
                      (traverse (second rest)))))))]
      (traverse (get fs ["/"]))
      @sizes)))

(defn solve [input]
  (let [lines (str/split-lines input)
        fs (build-fs lines)
        small-dirs (find-small-dirs fs 100000)]
    (reduce + small-dirs)))

;; Example usage
(def sample-input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(println (solve sample-input))
