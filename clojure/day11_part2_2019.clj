
(ns paint
  (:require [clojure.string :as str]))

(defn read-program [file]
  (->> (slurp file)
       str/trim
       (#(str/split % #","))
       (mapv parse-long)))

(defn computer [program]
  (let [mem (atom (vec program))
        ip  (atom 0)
        rb  (atom 0)
        in  (atom nil)
        out (atom [])]
    {:memory mem
     :ip ip
     :rb rb
     :input in
     :output out
     :halted (atom false)}))

(defn halted? [c] @(:halted c))

(defn get-mem [c addr]
  (get @(:memory c) addr 0))

(defn set-mem [c addr v]
  (let [m (:memory c)]
    (when (>= addr (count @m))
      (swap! m into (repeat (inc (- addr (count @m))) 0)))
    (swap! m assoc addr v)))

(defn param [c offset mode]
  (let [v (get-mem c (+ @(:ip c) offset))]
    (case mode
      0 (get-mem c v)
      1 v
      2 (get-mem c (+ @(:rb c) v)))))

(defn addr [c offset mode]
  (let [v (get-mem c (+ @(:ip c) offset))]
    (case mode
      0 v
      2 (+ @(:rb c) v))))

(defn step [c]
  (let [inst (get-mem c @(:ip c))
        op   (mod inst 100)
        m1   (mod (quot inst 100) 10)
        m2   (mod (quot inst 1000) 10)
        m3   (mod (quot inst 10000) 10)]
    (case op
      1 (do (set-mem c (addr c 3 m3)
                     (+ (param c 1 m1) (param c 2 m2)))
            (swap! (:ip c) + 4))
      2 (do (set-mem c (addr c 3 m3)
                     (* (param c 1 m1) (param c 2 m2)))
            (swap! (:ip c) + 4))
      3 (if (nil? @(:input c))
          :need-input
          (do (set-mem c (addr c 1 m1) @(:input c))
              (reset! (:input c) nil)
              (swap! (:ip c) + 2)))
      4 (let [v (param c 1 m1)]
          (swap! (:output c) conj v)
          (swap! (:ip c) + 2)
          (when (= (count @(:output c)) 2) @(:output c)))
      5 (swap! (:ip c)
               (if (not= (param c 1 m1) 0)
                 (constantly (param c 2 m2))
                 #(+ % 3)))
      6 (swap! (:ip c)
               (if (= (param c 1 m1) 0)
                 (constantly (param c 2 m2))
                 #(+ % 3)))
      7 (do (set-mem c (addr c 3 m3)
                     (if (< (param c 1 m1) (param c 2 m2)) 1 0))
            (swap! (:ip c) + 4))
      8 (do (set-mem c (addr c 3 m3)
                     (if (= (param c 1 m1) (param c 2 m2)) 1 0))
            (swap! (:ip c) + 4))
      9 (do (swap! (:rb c) + (param c 1 m1))
            (swap! (:ip c) + 2))
      99 (do (reset! (:halted c) true) :halted)
      (throw (Exception. (str "bad op " op))))))

(defn run [c input]
  (reset! (:input c) input)
  (reset! (:output c) [])
  (loop []
    (let [r (step c)]
      (cond
        (= r :halted) :halted
        (= r :need-input) :need-input
        (vector? r) r
        :else (recur)))))

(defn paint [program start-color]
  (let [c (computer program)
        hull (atom {})
        pos [0 0]
        dir [0 1]]
    (swap! hull assoc pos start-color)
    (loop [pos pos
           dir dir]
      (if (halted? c)
        @hull
        (let [color (get @hull pos 0)
              out (run c color)]
          (if (= out :halted)
            @hull
            (let [[paint-color turn] out
                  new-dir (if (zero? turn)
                            [(- (second dir)) (first dir)]
                            [(second dir) (- (first dir))])
                  new-pos (mapv + pos new-dir)]
              (swap! hull assoc pos paint-color)
              (recur new-pos new-dir))))))))

(defn print-hull [hull]
  (let [xs (map first (keys hull))
        ys (map second (keys hull))
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)]
    (doseq [y (range max-y (dec min-y) -1)]
      (doseq [x (range min-x (inc max-x))]
        (print (if (= (get hull [x y] 0) 1) \█ \space)))
      (println))))

(defn -main []
  (let [program (read-program "input.txt")
        hull1 (paint program 0)]
    (println "Part 1:" (count hull1))
    (let [hull2 (paint program 1)]
      (println "Part 2:")
      (print-hull hull2))))

(-main)
