(defn spiral-coords
  "Generate spiral coordinates"
  []
  (let [dirs [[0 1] [-1 0] [0 -1] [1 0]]]
    (loop [x 0, y 0, dir 0, steps 1, step-count 0, result []]
      (let [next-result (conj result [x y])
            [dx dy] (dirs dir)]
        (if (= (count next-result) 1000000)  ; Arbitrary large number
          next-result
          (if (= step-count steps)
            (let [new-dir (mod (inc dir) 4)
                  new-steps (if (even? new-dir) (inc steps) steps)]
              (recur (+ x dx) (+ y dy) new-dir new-steps 1 next-result))
            (recur (+ x dx) (+ y dy) dir steps (inc step-count) next-result)))))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn solve [input]
  (let [coords (spiral-coords)
        target-coord (nth coords (dec input))]
    (manhattan-distance target-coord [0 0])))

; Example usage:
; (solve 1024) ; Should return 31
