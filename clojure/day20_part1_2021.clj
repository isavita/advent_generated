
(defn parse-input [input]
  (let [[algorithm _ & image-lines] (clojure.string/split input #"\n")
        image (mapv vec image-lines)]
    {:algorithm algorithm
     :image image}))

(defn get-pixel [image x y default]
  (if (and (>= x 0) (< x (count (first image)))
           (>= y 0) (< y (count image)))
    (nth (nth image y) x)
    default))

(defn enhance-pixel [image algorithm x y default]
  (let [binary-string (apply str
                             (for [dy (range -1 2)
                                   dx (range -1 2)]
                               (if (= (get-pixel image (+ x dx) (+ y dy) default) \#) "1" "0")))
        index (Integer/parseInt binary-string 2)]
    (nth algorithm index)))

(defn enhance-image [data default]
  (let [{:keys [algorithm image]} data
        width (count (first image))
        height (count image)
        new-width (+ width 2)
        new-height (+ height 2)
        new-image (vec (for [y (range new-height)]
                         (vec (for [x (range new-width)]
                                (enhance-pixel image algorithm (- x 1) (- y 1) default)))))]
    new-image))

(defn solve [input iterations]
  (let [data (parse-input input)
        initial-default \.
        enhanced-image (nth
                         (iterate (fn [[img default]]
                                    (let [new-img (enhance-image {:algorithm (:algorithm data) :image img} default)
                                          new-default (if (= (first (:algorithm data)) \#)
                                                        (if (= default \#) (nth (:algorithm data) 511) (first (:algorithm data)))
                                                        (first (:algorithm data)))]
                                      [new-img new-default]))
                                  [(:image data) initial-default])
                         iterations)
        final-image (first enhanced-image)]
    (count (filter #{\#} (flatten final-image)))))

(defn main []
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve input 2))))

(main)
