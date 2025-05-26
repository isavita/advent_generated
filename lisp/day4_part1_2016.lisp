
(defun split-sequence (delimiter sequence)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter sequence :start start)
        collect (subseq sequence start (or end (length sequence)))
        while end))

(defun string-join (strings separator)
  (with-output-to-string (s)
    (loop for i from 0 below (length strings)
          for str = (elt strings i) do
      (write-string str s)
      (unless (= i (- (length strings) 1))
        (write-char separator s)))))

(defun is-real-room (name checksum)
  (let* ((cleaned-name (remove #\- name))
         (counts (make-hash-table :test 'eql)))
    (loop for char across cleaned-name do
      (incf (gethash char counts 0)))
    (let* ((sorted-chars
             (sort (loop for char being the hash-key of counts
                         collect char)
                   (lambda (c1 c2)
                     (let ((count1 (gethash c1 counts))
                           (count2 (gethash c2 counts)))
                       (cond ((> count1 count2) t)
                             ((< count1 count2) nil)
                             (t (char< c1 c2)))))))
           (top-5 (subseq sorted-chars 0 (min 5 (length sorted-chars)))))
      (string= checksum (coerce top-5 'string)))))

(defun decrypt-name (name shift)
  (loop for char across name
        collect (cond ((char= char #\-) #\ )
                      (t (code-char
                          (+ (char-code #\a)
                             (mod (+ (- (char-code char) (char-code #\a)) shift) 26)))))
        into decrypted-chars
        finally (return (coerce decrypted-chars 'string))))

(defun main ()
  (let ((total-sum 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line do
        (let* ((parts (split-sequence #\- line))
               (name (string-join (subseq parts 0 (- (length parts) 1)) #\-))
               (last-part (elt parts (- (length parts) 1)))
               (bracket-pos (position #\[ last-part))
               (sector-id-str (subseq last-part 0 bracket-pos))
               (checksum (subseq last-part (1+ bracket-pos) (- (length last-part) 1)))
               (sector-id (parse-integer sector-id-str)))
          (when (is-real-room name checksum)
            (incf total-sum sector-id)
            (decrypt-name name sector-id)))))
    (format t "~a~%" total-sum)))

(main)
