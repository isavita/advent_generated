
(defun read-decks (filename)
  (with-open-file (s filename)
    (read-line s)
    (let ((deck1 (loop for line = (read-line s nil nil)
                       while (and line (not (string= line "")))
                       collect (parse-integer line))))
      (read-line s)
      (let ((deck2 (loop for line = (read-line s nil nil)
                         while line
                         collect (parse-integer line))))
        (list deck1 deck2)))))

(defun calculate-score (deck)
  (loop for card in deck
        for weight downfrom (length deck)
        sum (* card weight)))

(defun play-game (deck1 deck2)
  (loop while (and deck1 deck2) do
    (let* ((card1 (car deck1))
           (card2 (car deck2)))
      (setf deck1 (cdr deck1))
      (setf deck2 (cdr deck2))
      (cond ((> card1 card2)
             (setf deck1 (append deck1 (list card1 card2))))
            (t
             (setf deck2 (append deck2 (list card2 card1)))))))
  (if deck1
      (calculate-score deck1)
      (calculate-score deck2)))

(defun main ()
  (let* ((decks (read-decks "input.txt"))
         (deck1 (car decks))
         (deck2 (cadr decks))
         (score (play-game deck1 deck2)))
    (format t "~a~%" score)))

(main)
