
(ns disk-fragmenter
  (:require [clojure.string :as str]))

(defn parse-disk-map [disk-map-str]
  (loop [s disk-map-str
         file-id 0
         result []]
    (if (empty? s)
      result
      (let [size (Character/digit (first s) 10)
            remaining (rest s)]
        (if (even? file-id)  ; File
          (recur remaining (inc file-id) (conj result {:type :file :size size :id (/ file-id 2)}))
          (recur remaining (inc file-id) (conj result {:type :free :size size})))))))

(defn generate-initial-blocks [parsed-map]
    (reduce
     (fn [blocks segment]
       (if (= :file (:type segment))
         (let [id (:id segment)]
           (into blocks (repeat (:size segment) id)))
         (into blocks (repeat (:size segment) \.))))
     []
     parsed-map))


(defn compact-disk [initial-blocks]
  (loop [blocks initial-blocks
         moved? true]  ; Add a flag to track if any moves occurred
    (if-not moved?
      blocks
      (let [first-free-index (first (keep-indexed #(when (= \. %2) %1) blocks))
            last-file-index (->> (keep-indexed #(when (number? %2) %1) blocks)
                                 (reduce max -1))] 
        (if (and (some? first-free-index) (> last-file-index first-free-index))
          (recur (assoc blocks
                        first-free-index (nth blocks last-file-index)
                        last-file-index \.)
                 true) ; Set moved? to true since a move occurred
          blocks)))))

(defn calculate-checksum [compacted-blocks]
  (reduce + (keep-indexed (fn [idx block]
                             (when (number? block)
                               (* idx block)))
                           compacted-blocks)))

(defn -main []
  (let [disk-map-str (str/trim (slurp "input.txt"))
        parsed-map (parse-disk-map disk-map-str)
        initial-blocks (generate-initial-blocks parsed-map)
        compacted-blocks (compact-disk initial-blocks)
        checksum (calculate-checksum compacted-blocks)]
    (println checksum)))

(-main)
