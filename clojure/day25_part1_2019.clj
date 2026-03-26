
(ns solution
  (:require [clojure.string]
            [clojure.set]))

(defn decode [instr]
  [(mod instr 100) (mod (quot instr 100) 10) (mod (quot instr 1000) 10) (mod (quot instr 10000) 10)])

(defn step [{:keys [mem ip rb in out] :as vm}]
  (let [[op m1 m2 m3] (decode (get mem ip 0))
        get-p (fn [off mode] (let [v (get mem (+ ip off) 0)] (case mode 0 (get mem v 0) 1 v 2 (get mem (+ rb v) 0))))
        get-a (fn [off mode] (let [v (get mem (+ ip off) 0)] (case mode 0 v 2 (+ rb v))))]
    (case op
      1 (assoc vm :mem (assoc mem (get-a 3 m3) (+ (get-p 1 m1) (get-p 2 m2))) :ip (+ ip 4))
      2 (assoc vm :mem (assoc mem (get-a 3 m3) (* (get-p 1 m1) (get-p 2 m2))) :ip (+ ip 4))
      3 (if (empty? in) (assoc vm :status :waiting) (assoc vm :mem (assoc mem (get-a 1 m1) (first in)) :in (subvec in 1) :ip (+ ip 2)))
      4 (assoc vm :out (conj out (get-p 1 m1)) :ip (+ ip 2))
      5 (assoc vm :ip (if (not= (get-p 1 m1) 0) (get-p 2 m2) (+ ip 3)))
      6 (assoc vm :ip (if (= (get-p 1 m1) 0) (get-p 2 m2) (+ ip 3)))
      7 (assoc vm :mem (assoc mem (get-a 3 m3) (if (< (get-p 1 m1) (get-p 2 m2)) 1 0)) :ip (+ ip 4))
      8 (assoc vm :mem (assoc mem (get-a 3 m3) (if (= (get-p 1 m1) (get-p 2 m2)) 1 0)) :ip (+ ip 4))
      9 (assoc vm :rb (+ rb (get-p 1 m1)) :ip (+ ip 2))
      99 (assoc vm :status :halted))))

(defn run-vm [vm]
  (loop [s (assoc vm :status :running :out [])]
    (if (or (= (:status s) :waiting) (= (:status s) :halted)) s (recur (step s)))))

(defn get-list [lines header]
  (->> lines (map clojure.string/trim) (drop-while #(not (clojure.string/includes? % header))) (rest) (take-while #(clojure.string/starts-with? % "- ")) (map #(subs % 2))))

(defn parse-output [output]
  (let [lines (clojure.string/split-lines output)]
    {:room (some #(second (re-find #"== (.+) ==" %)) lines)
     :items (get-list lines "Items here:")
     :doors (get-list lines "Doors here lead:")
     :alert? (some #(clojure.string/includes? % "A loud, robotic voice says \"Alert!") lines)
     :password (second (re-find #"typing (\d+) on the keypad" output))}))

(def opposite {"north" "south", "south" "north", "west" "east", "east" "west"})
(def blacklist #{"photons" "escape pod" "molten lava" "infinite loop" "giant electromagnet"})

(defn find-path [rooms start target]
  (loop [q (conj (clojure.lang.PersistentQueue/EMPTY) [start []]) v #{start}]
    (if (empty? q) nil
        (let [[curr p] (peek q)]
          (if (= curr target) p
              (let [nxts (for [[d n] (:doors (get rooms curr)) :when (and n (not= n :floor) (not (v n)))] [n (conj p d)])]
                (recur (reduce conj (pop q) nxts) (into v (map first nxts)))))))))

(defn get-unexplored [rooms room] (when room (some (fn [[d n]] (when (nil? n) d)) (:doors (get rooms room)))))

(defn run []
  (let [mem (zipmap (range) (map #(Long/parseLong %) (clojure.string/split (clojure.string/trim (slurp "input.txt")) #",")))]
    (loop [vm {:mem mem :ip 0 :rb 0 :in [] :out [] :status :running}
           world {:rooms {} :path [] :mode 0 :holding #{} :current-room nil :last-dir nil :last-room nil :mask 0 :brute-items []}]
      (let [vmo (run-vm vm)
            out (apply str (map char (:out vmo)))
            p (parse-output out)
            curr (cond (:alert? p) (:current-room world) (:room p) (:room p) :else (:current-room world))
            rms (let [r (:rooms world)
                      r (if (and curr (not (get r curr))) (assoc r curr {:doors (zipmap (:doors p) (repeat nil)) :items (:items p)}) r)]
                  (if (and (:last-room world) (:last-dir world))
                    (if (:alert? p) (assoc-in r [(:last-room world) :doors (:last-dir world)] :floor)
                        (let [opp (opposite (:last-dir world))]
                          (-> r (assoc-in [(:last-room world) :doors (:last-dir world)] curr) (assoc-in [curr :doors opp] (:last-room world))))) r))
            world (assoc world :rooms rms :current-room curr)]
        (if-let [pass (:password p)] (println pass)
          (cond
            (= (:mode world) 0)
            (let [safe (remove blacklist (:items p))
                  unexp (get-unexplored rms curr)]
              (cond (seq safe) (let [i (first safe)] (recur (assoc vmo :in (mapv int (str "take " i "\n"))) (-> world (update :holding conj i) (assoc :last-dir nil :last-room nil))))
                    unexp (recur (assoc vmo :in (mapv int (str unexp "\n"))) (-> world (assoc :last-dir unexp :last-room curr) (update :path conj curr)))
                    (seq (:path world)) (let [prev (last (:path world)) back (some (fn [[d r]] (when (= r prev) d)) (:doors (get rms curr)))]
                                          (recur (assoc vmo :in (mapv int (str back "\n"))) (-> world (assoc :last-dir back :last-room curr) (update :path pop))))
                    :else (let [cp (some (fn [[rn d]] (when (some #(= % :floor) (vals (:doors d))) rn)) rms)
                                td (some (fn [[d r]] (when (= r :floor) d)) (:doors (get rms cp)))]
                            (recur (assoc vmo :out []) (assoc world :mode 1 :checkpoint cp :test-dir td :target-path (find-path rms curr cp))))))
            (= (:mode world) 1)
            (if (seq (:target-path world)) (let [d (first (:target-path world))] (recur (assoc vmo :in (mapv int (str d "\n"))) (assoc world :target-path (rest (:target-path world)) :last-dir d :last-room curr)))
                (recur (assoc vmo :out []) (assoc world :mode 2 :brute-items (vec (:holding world)) :mask 0)))
            (= (:mode world) 2)
            (let [target (set (keep-indexed (fn [i itm] (when (bit-test (:mask world) i) itm)) (:brute-items world)))
                  holding (:holding world)]
              (cond (not= target holding) (let [drp (first (clojure.set/difference holding target)) tke (first (clojure.set/difference target holding))]
                                            (if drp (recur (assoc vmo :in (mapv int (str "drop " drp "\n"))) (-> world (update :holding disj drp) (assoc :last-dir nil :last-room nil)))
                                                (recur (assoc vmo :in (mapv int (str "take " tke "\n"))) (-> world (update :holding conj tke) (assoc :last-dir nil :last-room nil)))))
                    :else (recur (assoc vmo :in (mapv int (str (:test-dir world) "\n"))) (-> world (update :mask inc) (assoc :last-dir (:test-dir world) :last-room curr)))))))))))

(run)
