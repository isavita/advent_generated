
(require '[clojure.string :as str])

(defn solve []
  (let [instrs (mapv #(mapv (fn [s] (if (re-matches #"-?\d+" s) (Long/parseLong s) s))
                            (str/split (str/trim %) #"\s+"))
                     (str/split-lines (str/trim (slurp "input.txt"))))
        gv (fn [regs x] (if (number? x) x (get regs x 0)))
        run (fn [state pid]
              (let [other (if (= pid :p0) :p1 :p0)]
                (loop [s state]
                  (let [{:keys [regs ip q]} (get s pid)
                        [op x y] (get instrs ip)]
                    (if (nil? op)
                      s
                      (case op
                        "snd" (recur (-> s (update-in [other :q] conj (gv regs x))
                                         (update-in [pid :ip] inc)
                                         (cond-> (= pid :p1) (update :sc inc))))
                        "set" (recur (-> s (assoc-in [pid :regs x] (gv regs y)) (update-in [pid :ip] inc)))
                        "add" (recur (-> s (assoc-in [pid :regs x] (+ (gv regs x) (gv regs y))) (update-in [pid :ip] inc)))
                        "mul" (recur (-> s (assoc-in [pid :regs x] (* (gv regs x) (gv regs y))) (update-in [pid :ip] inc)))
                        "mod" (recur (-> s (assoc-in [pid :regs x] (mod (gv regs x) (gv regs y))) (update-in [pid :ip] inc)))
                        "rcv" (if (empty? q)
                                s
                                (recur (-> s (assoc-in [pid :regs x] (peek q))
                                           (update-in [pid :q] pop)
                                           (update-in [pid :ip] inc))))
                        "jgz" (recur (update-in s [pid :ip] + (if (> (gv regs x) 0) (gv regs y) 1)))))))))]
    (loop [s {:p0 {:regs {"p" 0} :ip 0 :q clojure.lang.PersistentQueue/EMPTY}
              :p1 {:regs {"p" 1} :ip 0 :q clojure.lang.PersistentQueue/EMPTY}
              :sc 0}]
      (let [s' (-> s (run :p0) (run :p1))]
        (if (= s s')
          (println (:sc s))
          (recur s'))))))

(solve)
