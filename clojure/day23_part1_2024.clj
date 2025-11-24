
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn -main [& _]
  (let [adj (atom {})
        names (atom [])
        name->id (atom {})]
    ;; parse file
    (with-open [rdr (io/reader "input.txt")]
      (doseq [line (line-seq rdr)]
        (when-let [[_ a b] (re-matches #"([^-]+)-(.+)" line)]
          (let [id-a (or (@name->id a)
                         (let [id (count @names)]
                           (swap! names conj a)
                           (swap! name->id assoc a id)
                           id))
                id-b (or (@name->id b)
                        (let [id (count @names)]
                          (swap! names conj b)
                          (swap! name->id assoc b id)
                          id))]
            (swap! adj update id-a (fnil conj #{}) id-b)
            (swap! adj update id-b (fnil conj #{}) id-a)))))
    ;; count triangles
    (let [n (count @names)
          adj @adj
          names @names]
      (println
       (count
        (for [i (range n)
              j (range (inc i) n)
              :when (contains? (adj i) j)
              k (range (inc j) n)
              :when (and (contains? (adj j) k)
                         (contains? (adj k) i)
                         (or (= \t (first (names i)))
                             (= \t (first (names j)))
                             (= \t (first (names k)))))]
          1))))))

(-main)
