
(ns solution (:require [clojure.string :as str]))

(defn solve-bp [bp limit]
  (let [mo (max (:oo bp) (:oc bp) (:oob bp) (:og bp))
        mc (:cob bp) mb (:obg bp)]
    (loop [t 0 states #{[0 0 0 0 1 0 0 0]}]
      (if (= t limit)
        (reduce max 0 (map #(nth % 3) states))
        (let [rem (- limit t)
              nxt (reduce (fn [acc [o c b g ro rc rb rg]]
                            (if (and (>= o (:og bp)) (>= b (:obg bp)))
                              (conj acc [(min (+ o ro (- (:og bp))) (* rem mo))
                                         (min (+ c rc) (* rem mc))
                                         (min (+ b rb (- (:obg bp))) (* rem mb))
                                         (+ g rg) ro rc rb (inc rg)])
                              (let [s (conj acc [(min (+ o ro) (* rem mo))
                                                 (min (+ c rc) (* rem mc))
                                                 (min (+ b rb) (* rem mb))
                                                 (+ g rg) ro rc rb rg])]
                                (cond-> s
                                  (and (>= o (:oob bp)) (>= c (:cob bp)) (< rb mb))
                                  (conj [(min (+ o ro (- (:oob bp))) (* rem mo))
                                         (min (+ c rc (- (:cob bp))) (* rem mc))
                                         (min (+ b rb) (* rem mb))
                                         (+ g rg) ro rc (inc rb) rg])
                                  (and (>= o (:oc bp)) (< rc mc))
                                  (conj [(min (+ o ro (- (:oc bp))) (* rem mo))
                                         (min (+ c rc) (* rem mc))
                                         (min (+ b rb) (* rem mb))
                                         (+ g rg) ro (inc rc) rb rg])
                                  (and (>= o (:oo bp)) (< ro mo))
                                  (conj [(min (+ o ro (- (:oo bp))) (* rem mo))
                                         (min (+ c rc) (* rem mc))
                                         (min (+ b rb) (* rem mb))
                                         (+ g rg) (inc ro) rc rb rg])))))
                          #{} states)]
          (recur (inc t) (if (> (count nxt) 1000)
                           (set (take 1000 (sort-by (fn [[o c b g ro rc rb rg]] [g rg b rb c rc o ro]) #(compare %2 %1) nxt)))
                           nxt)))))))

(let [bps (map (fn [l] (let [n (map #(Integer/parseInt %) (re-seq #"\d+" l))]
                        {:id (nth n 0) :oo (nth n 1) :oc (nth n 2) :oob (nth n 3)
                         :cob (nth n 4) :og (nth n 5) :obg (nth n 6)}))
               (str/split-lines (slurp "input.txt")))]
  (println (reduce + (map #(* (:id %) (solve-bp % 24)) bps))))

