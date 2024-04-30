(ns solution
  (:import (java.io BufferedReader FileReader))
  (:import (java.lang Math)))

(defn main []
  (with-open [r (BufferedReader. (FileReader. "input.txt"))]
    (let [target (Integer/parseInt (.trim (first (line-seq r))))]
      (let [side-length (int (Math/ceil (Math/sqrt target)))]
        (if (even? side-length) (inc side-length) side-length)
        (let [max-value (* side-length side-length)
              steps-from-edge (quot (dec side-length) 2)
              distance-to-middle (apply min (for [i (range 4)]
                                             (let [middle-point (- max-value steps-from-edge (* (dec side-length) i))]
                                               (Math/abs (- target middle-point)))))]
          (println (+ steps-from-edge distance-to-middle)))))))

(main)