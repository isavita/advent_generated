(require '[clojure.string :as str])

(defn parse-input [input]
  (reduce (fn [acc line]
            (let [[_ prereq step] (re-find #"Step (\w) must be finished before step (\w) can begin." line)]
              (-> acc
                  (update :deps #(update % step (fnil conj #{}) prereq))
                  (update :all-steps #(conj % step prereq)))))
          {:deps {}, :all-steps #{}}
          (str/split-lines input)))

(defn find-order [input]
  (let [{:keys [deps all-steps]} (parse-input input)
        initial-available (remove #(contains? deps %) all-steps)]
    (loop [order []
           available (into (sorted-set) initial-available)
           remaining-deps deps]
      (if (empty? available)
        (str/join order)
        (let [next-step (first available)
              new-available (for [[step prereqs] remaining-deps
                                  :when (and (prereqs next-step)
                                             (every? (set order) (disj prereqs next-step)))]
                              step)]
          (recur (conj order next-step)
                 (into (disj available next-step) new-available)
                 (dissoc remaining-deps next-step)))))))

;; Example usage:
;; (def sample-input "Step C must be finished before step A can begin.
;; Step C must be finished before step F can begin.
;; Step A must be finished before step B can begin.
;; Step A must be finished before step D can begin.
;; Step B must be finished before step E can begin.
;; Step D must be finished before step E can begin.
;; Step F must be finished before step E can begin.")
;; 
;; (println (find-order sample-input))
