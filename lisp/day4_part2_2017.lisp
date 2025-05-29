
(defconstant +input-file+ "input.txt")

(defun split-string (s char)
  (loop for start = 0 then (1+ end)
        for end = (position char s :start start)
        collect (subseq s start end)
        while end))

(defun sort-string (s)
  (coerce (sort (coerce s 'list) #'char<) 'string))

(defun is-valid-passphrase (passphrase)
  (let* ((words (split-string passphrase #\Space)))
    (= (length words) (length (remove-duplicates words :test #'string=)))))

(defun is-valid-passphrase-part-two (passphrase)
  (let* ((words (split-string passphrase #\Space))
         (sorted-words (mapcar #'sort-string words)))
    (= (length sorted-words) (length (remove-duplicates sorted-words :test #'string=)))))

(defun count-valid-passphrases (passphrases validation-fn)
  (count-if validation-fn passphrases))

(defun main ()
  (let ((input-lines (with-open-file (in +input-file+)
                       (loop for line = (read-line in nil nil)
                             while line
                             collect line))))

    (let ((part-one-count (count-valid-passphrases input-lines #'is-valid-passphrase)))
      (format t "Part One: ~a~%" part-one-count))

    (let ((part-two-count (count-valid-passphrases input-lines #'is-valid-passphrase-part-two)))
      (format t "Part Two: ~a~%" part-two-count))))

(main)
