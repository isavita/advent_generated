
(defn most-common-char [chars]
  (->> chars
       (frequencies)
       (sort-by val >)
       first
       key))

(defn get-error-corrected-message [messages]
  (apply str (map most-common-char (apply map vector messages))))

(def input (slurp "input.txt"))
(def messages (mapv seq (clojure.string/split-lines input)))
(println (get-error-corrected-message messages))
