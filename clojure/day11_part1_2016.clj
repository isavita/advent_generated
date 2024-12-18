
(ns radioisotope-generators
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        parse-line (fn [line]
                     (let [[_ floor items] (re-matches #"The (\w+) floor contains (.*)\." line)]
                       (if (not (nil? items))
                         (let [item-strs (str/split items #", | and ")
                               parse-item (fn [item-str]
                                            (cond
                                              (str/includes? item-str "generator")
                                              (let [[_ type] (re-matches #"a (\w+)-compatible generator" item-str)]
                                                [:generator (keyword type)])
                                              (str/includes? item-str "microchip")
                                              (let [[_ type] (re-matches #"a (\w+)-compatible microchip" item-str)]
                                                [:microchip (keyword type)])
                                              :else nil))]
                           (->> item-strs
                                (remove nil?)
                                (map parse-item)
                                (remove nil?)))
                         [])))]
    (->> lines
         (map parse-line)
         (zipmap (range 1 5)))))

(defn is-valid-state? [state]
  (let [floors (vals state)
        is-fried? (fn [floor]
                    (let [generators (filter (fn [[type _]] (= type :generator)) floor)
                          microchips (filter (fn [[type _]] (= type :microchip)) floor)]
                      (some (fn [[_ chip-type]]
                              (some (fn [[_ gen-type]]
                                      (not= chip-type gen-type))
                                    generators))
                            microchips)))]
    (not (some is-fried? floors))))

(defn get-possible-moves [state elevator-floor]
  (let [current-floor (get state elevator-floor)
        items-on-floor (set current-floor)
        possible-moves (for [i (range 1 5)
                             :when (not= i elevator-floor)
                             items (set/subsets items-on-floor)
                             :when (and (not (empty? items)) (<= (count items) 2))]
                         {:to-floor i :items items})]
    possible-moves))

(defn apply-move [state elevator-floor move]
  (let [{:keys [to-floor items]} move
        new-state (reduce (fn [acc item]
                            (update acc elevator-floor (fn [floor] (remove #(= item %) floor))))
                          state
                          items)
        new-state (update new-state to-floor (fn [floor] (concat floor items)))]
    {:state new-state :elevator-floor to-floor}))

(defn solve [initial-state]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY {:state initial-state :elevator-floor 1 :steps 0})
         visited #{}]
    (if (empty? queue)
      nil
      (let [{:keys [state elevator-floor steps]} (peek queue)
            queue (pop queue)
            state-key [state elevator-floor]]
        (if (contains? visited state-key)
          (recur queue visited)
          (let [visited (conj visited state-key)
                all-on-fourth? (every? (fn [floor] (empty? floor)) (subvec (vals state) 0 3))
                ]
            (if all-on-fourth?
              steps
              (let [possible-moves (get-possible-moves state elevator-floor)
                    next-states (->> possible-moves
                                     (map (fn [move] (apply-move state elevator-floor move)))
                                     (filter (fn [{:keys [state]}] (is-valid-state? state))))
                    next-queue (reduce (fn [q {:keys [state elevator-floor]}]
                                        (conj q {:state state :elevator-floor elevator-floor :steps (inc steps)}))
                                      queue
                                      next-states)]
                (recur next-queue visited)))))))))

(defn -main [& args]
  (let [input (slurp "input.txt")
        initial-state (parse-input input)]
    (println (solve initial-state))))
