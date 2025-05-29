
(defvar *graph* (make-hash-table :test 'equal))
(defvar *best-clique* nil)

(defun neighbors-of (node)
  (gethash node *graph* (make-hash-table :test 'equal)))

(defun intersect-lists (lst ht)
  (loop for x in lst
        when (gethash x ht)
        collect x))

(defun add-to-list (lst item)
  (append lst (list item)))

(defun remove-from-list (lst item)
  (remove item lst :test 'equal))

(defun bron-kerbosch (r p x)
  (when (and (endp p) (endp x))
    (when (> (length r) (length *best-clique*))
      (setf *best-clique* (copy-list r)))
    (return-from bron-kerbosch))

  (let* ((pivot (if (not (endp p)) (car p) (car x)))
         (pivot-neighbors (neighbors-of pivot)))

    (loop for v in (copy-list (loop for node in p
                                    unless (gethash node pivot-neighbors)
                                    collect node))
          do
             (let ((neighbors-v (neighbors-of v)))
               (bron-kerbosch (add-to-list r v)
                              (intersect-lists p neighbors-v)
                              (intersect-lists x neighbors-v)))
             (setf p (remove-from-list p v))
             (setf x (add-to-list x v)))))

(defun split-string (string delimiter)
  (loop with result = nil
        with start = 0
        for i from 0 below (length string)
        when (char= (char string i) delimiter)
        do (progn
             (push (subseq string start i) result)
             (setf start (1+ i)))
        finally (progn
                  (push (subseq string start) result)
                  (return (nreverse result)))))

(defun main ()
  (setf *graph* (make-hash-table :test 'equal))
  (setf *best-clique* nil)

  (let ((nodes-set (make-hash-table :test 'equal)))
    (with-open-file (stream "input.txt" :direction :input
                            :if-does-not-exist :error)
      (loop for line = (read-line stream nil nil)
            while line
            do
               (let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
                      (parts (split-string trimmed-line #\-)))
                 (when (= (length parts) 2)
                   (let ((a (first parts))
                         (b (second parts)))
                     (unless (gethash a *graph*) (setf (gethash a *graph*) (make-hash-table :test 'equal)))
                     (unless (gethash b *graph*) (setf (gethash b *graph*) (make-hash-table :test 'equal)))
                     
                     (setf (gethash b (gethash a *graph*)) t)
                     (setf (gethash a (gethash b *graph*)) t)

                     (setf (gethash a nodes-set) t)
                     (setf (gethash b nodes-set) t))))))

    (let ((all-nodes (loop for k being the hash-key of nodes-set collect k)))
      (bron-kerbosch nil all-nodes nil))

    (setf *best-clique* (sort *best-clique* #'string<))
    
    (format t "~{~a~^,~}~%" *best-clique*)))

(main)
