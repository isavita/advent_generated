(ns main
  (:require [clojure.string :as str]))

(defn read-program []
  (mapv bigint (map #(Long/parseLong %) (str/split (slurp "input.txt") #"[,\s]+"))))

(defn mem-get [mem addr]
  (get mem addr 0N))

(defn mem-set [mem addr val]
  (assoc mem addr val))

(defn make-comp [program id]
  {:mem (reduce-kv (fn [m i v] (assoc m (bigint i) v)) {} program)
   :ip 0N
   :rb 0N
   :inputs [(bigint id)]
   :in-idx 0
   :outputs []
   :out-idx 0
   :halted false
   :idle false})

(defn param [comp mode off]
  (let [mem (:mem comp)
        ip (:ip comp)
        rb (:rb comp)
        v (mem-get mem (+ ip off))]
    (case mode
      0 (mem-get mem v)
      1 v
      2 (mem-get mem (+ rb v)))))

(defn write-addr [comp mode off]
  (let [mem (:mem comp)
        ip (:ip comp)
        rb (:rb comp)
        v (mem-get mem (+ ip off))]
    (case mode
      0 v
      2 (+ rb v))))

(defn run-comp [comp]
  (loop [c comp]
    (let [instr (mem-get (:mem c) (:ip c))
          op (mod instr 100)
          m1 (mod (quot instr 100) 10)
          m2 (mod (quot instr 1000) 10)
          m3 (mod (quot instr 10000) 10)]
      (case op
        99 (assoc c :halted true)
        1 (recur (-> c
                     (update :mem mem-set (write-addr c m3 3) (+ (param c m1 1) (param c m2 2)))
                     (update :ip + 4N)))
        2 (recur (-> c
                     (update :mem mem-set (write-addr c m3 3) (* (param c m1 1) (param c m2 2)))
                     (update :ip + 4N)))
        3 (if (< (:in-idx c) (count (:inputs c)))
            (let [v (nth (:inputs c) (:in-idx c))]
              (recur (-> c
                         (update :mem mem-set (write-addr c m1 1) v)
                         (update :ip + 2N)
                         (update :in-idx inc)
                         (assoc :idle false))))
            (-> c
                (update :mem mem-set (write-addr c m1 1) -1N)
                (update :ip + 2N)
                (assoc :inputs [] :in-idx 0 :idle true)))
        4 (let [v (param c m1 1)
                c2 (-> c
                       (update :outputs conj v)
                       (update :ip + 2N))]
            (if (>= (- (count (:outputs c2)) (:out-idx c2)) 3)
              c2
              (recur c2)))
        5 (recur (if (not= 0N (param c m1 1))
                   (assoc c :ip (param c m2 2))
                   (update c :ip + 3N)))
        6 (recur (if (= 0N (param c m1 1))
                   (assoc c :ip (param c m2 2))
                   (update c :ip + 3N)))
        7 (recur (-> c
                     (update :mem mem-set (write-addr c m3 3) (if (< (param c m1 1) (param c m2 2)) 1N 0N))
                     (update :ip + 4N)))
        8 (recur (-> c
                     (update :mem mem-set (write-addr c m3 3) (if (= (param c m1 1) (param c m2 2)) 1N 0N))
                     (update :ip + 4N)))
        9 (recur (-> c
                     (update :rb + (param c m1 1))
                     (update :ip + 2N)))
        nil))))

(defn process-outputs [comp queues nat]
  (loop [c comp q queues n nat]
    (if (>= (- (count (:outputs c)) (:out-idx c)) 3)
      (let [i (:out-idx c)
            dest (nth (:outputs c) i)
            x (nth (:outputs c) (inc i))
            y (nth (:outputs c) (+ i 2))
            q2 (if (= dest 255N)
                 q
                 (assoc q (int dest) (conj (get q (int dest)) [x y])))
            n2 (if (= dest 255N) [x y] n)]
        (recur (assoc c :out-idx (+ i 3)) q2 n2))
      [c q n])))

(defn all-idle? [comps queues]
  (and (every? empty? queues)
       (every? :idle comps)))

(defn main []
  (let [program (read-program)
        comps (vec (map #(make-comp program %) (range 50)))
        queues (vec (repeat 50 []))]
    (loop [cs comps qs queues nat nil prev nil]
      (let [[cs2 qs2 nat2]
            (reduce
             (fn [[acc-cs acc-qs natv] i]
               (let [c0 (nth acc-cs i)
                     c1 (if-let [pkt (first (get acc-qs i))]
                          (-> c0
                              (update :inputs into pkt)
                              (assoc :idle false)
                              (assoc :in-idx (count (:inputs c0))))
                          (if (< (:in-idx c0) (count (:inputs c0)))
                            c0
                            (update c0 :inputs conj -1N)))
                     qs3 (if-let [pkt (first (get acc-qs i))]
                           (update acc-qs i subvec 1)
                           acc-qs)
                     c2 (run-comp c1)
                     [c3 qs4 nat3] (process-outputs c2 qs3 natv)
                     cs3 (assoc acc-cs i c3)]
                 [cs3 qs4 nat3]))
             [cs qs nat]
             (range 50))]
        (if (and (all-idle? cs2 qs2) nat2)
          (let [[x y] nat2
                qs3 (update qs2 0 conj [x y])]
            (if (= y prev)
              (println y)
              (recur cs2 qs3 nat2 y)))
          (recur cs2 qs2 nat2 prev))))))

(main)