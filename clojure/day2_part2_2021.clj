(ns day2
  (:require [clojure.java.io :as io]))

(defn parse-command [line]
  (let [[direction value] (clojure.string/split line #" ")]
    [(keyword direction) (Integer/parseInt value)]))

(defn process-commands [commands]
  (reduce (fn [{:keys [horizontal depth aim] :as state} [command value]]
             (case command
               :forward {:horizontal (+ horizontal value)
                         :depth (+ depth (* aim value))
                         :aim aim}
               :down {:horizontal horizontal
                      :depth depth
                      :aim (+ aim value)}
               :up {:horizontal horizontal
                     :depth depth
                     :aim (- aim value)}))
           {:horizontal 0 :depth 0 :aim 0}
           (map parse-command commands)))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [commands (line-seq rdr)
          {:keys [horizontal depth]} (process-commands commands)]
      (println (* horizontal depth)))))

(-main)