
(ns not-enough-minerals
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defrecord Blueprint [id ore-cost clay-ore-cost obsidian-ore-cost obsidian-clay-cost geode-ore-cost geode-obsidian-cost])
(defrecord State [ore clay obsidian geode ore-robots clay-robots obsidian-robots geode-robots time-left])

(defn max-geode [^Blueprint bp ^State start]
  (let [max-ore-cost (max (.ore-cost bp) (.clay-ore-cost bp) (.obsidian-ore-cost bp) (.geode-ore-cost bp))
        queue (atom (clojure.lang.PersistentQueue/EMPTY))
        visited (atom #{})
        best (atom 0)]
    (swap! queue conj start)
    (while (seq @queue)
      (let [s ^State (peek @queue)]
        (swap! queue pop)
        (swap! best max (.geode s))
        (when (pos? (.time-left s))
          (let [ore-robots  (min (.ore-robots s)  max-ore-cost)
                clay-robots (min (.clay-robots s) (.obsidian-clay-cost bp))
                obs-robots  (min (.obsidian-robots s) (.geode-obsidian-cost bp))
                ore-needed  (* (.time-left s) max-ore-cost)
                ore         (min (.ore s) (- ore-needed (* (.ore-robots s) (dec (.time-left s)))))
                clay-needed (* (.time-left s) (.obsidian-clay-cost bp))
                clay        (min (.clay s) (- clay-needed (* (.clay-robots s) (dec (.time-left s)))))
                obs-needed  (* (.time-left s) (.geode-obsidian-cost bp))
                obsidian    (min (.obsidian s) (- obs-needed (* (.obsidian-robots s) (dec (.time-left s)))))]
            (let [trimmed (State. ore clay obsidian (.geode s) ore-robots clay-robots obs-robots (.geode-robots s) (.time-left s))]
              (when-not (contains? @visited trimmed)
                (swap! visited conj trimmed)
                (let [ore+  (+ (.ore s)  (.ore-robots s))
                      clay+ (+ (.clay s) (.clay-robots s))
                      obs+  (+ (.obsidian s) (.obsidian-robots s))
                      geo+  (+ (.geode s) (.geode-robots s))
                      t-    (dec (.time-left s))]
                  (swap! queue conj (State. ore+ clay+ obs+ geo+ (.ore-robots s) (.clay-robots s) (.obsidian-robots s) (.geode-robots s) t-))
                  (when (>= (.ore s) (.ore-cost bp))
                    (swap! queue conj (State. (- ore+  (.ore-cost bp))  clay+ obs+ geo+ (inc (.ore-robots s)) (.clay-robots s) (.obsidian-robots s) (.geode-robots s) t-)))
                  (when (>= (.ore s) (.clay-ore-cost bp))
                    (swap! queue conj (State. (- ore+  (.clay-ore-cost bp)) clay+ obs+ geo+ (.ore-robots s) (inc (.clay-robots s)) (.obsidian-robots s) (.geode-robots s) t-)))
                  (when (and (>= (.ore s) (.obsidian-ore-cost bp)) (>= (.clay s) (.obsidian-clay-cost bp)))
                    (swap! queue conj (State. (- ore+  (.obsidian-ore-cost bp)) (- clay+ (.obsidian-clay-cost bp)) obs+ geo+ (.ore-robots s) (.clay-robots s) (inc (.obsidian-robots s)) (.geode-robots s) t-)))
                  (when (and (>= (.ore s) (.geode-ore-cost bp)) (>= (.obsidian s) (.geode-obsidian-cost bp)))
                    (swap! queue conj (State. (- ore+  (.geode-ore-cost bp)) clay+ (- obs+ (.geode-obsidian-cost bp)) geo+ (.ore-robots s) (.clay-robots s) (.obsidian-robots s) (inc (.geode-robots s)) t-))))))))))
    @best))

(defn parse-blueprints []
  (with-open [rdr (io/reader "input.txt")]
    (doall
      (for [line (line-seq rdr)]
        (let [nums (mapv #(Long/parseLong %) (re-seq #"\d+" line))]
          (Blueprint. (nums 0) (nums 1) (nums 2) (nums 3) (nums 4) (nums 5) (nums 6)))))))

(defn -main [& _]
  (let [bps (parse-blueprints)
        init (State. 0 0 0 0 1 0 0 0 32)]
    (println (transduce (comp (map #(max-geode % init)) (take 3)) * bps))))

(-main)
