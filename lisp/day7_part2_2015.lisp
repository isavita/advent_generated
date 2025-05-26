
(defpackage :circuit-solver
  (:use :cl)
  (:export :main))

(in-package :circuit-solver)

(defun split-string-by-char (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i (or j (length string)))
        while j))

(defun split-string-by-substring (string substring)
  (let ((parts '())
        (start 0)
        (sub-len (length substring)))
    (loop
      (let ((pos (search substring string :start2 start)))
        (if pos
            (progn
              (push (subseq string start pos) parts)
              (setf start (+ pos sub-len)))
            (progn
              (push (subseq string start) parts)
              (return)))
        ))
    (nreverse parts)))

(defun read-file-content (filepath)
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (let* ((length (file-length stream))
           (content (make-string length)))
      (read-sequence content stream)
      (string-trim '(#\Space #\Tab #\Newline #\Return) content))))

(defun memo-dfs (graph entry memo)
  (multiple-value-bind (value foundp) (gethash entry memo)
    (when foundp
      (return-from memo-dfs value)))

  (let ((parsed-num (ignore-errors (parse-integer entry))))
    (when (and parsed-num (typep parsed-num 'integer))
      (return-from memo-dfs parsed-num)))

  (let* ((source-rule (gethash entry graph))
         (parts (split-string-by-char source-rule #\Space))
         (result 0))
    (setf result
          (cond
            ((= (length parts) 1)
             (memo-dfs graph (first parts) memo))
            ((string= (first parts) "NOT")
             (logxor (memo-dfs graph (second parts) memo) 65535))
            ((string= (second parts) "AND")
             (logand (memo-dfs graph (first parts) memo)
                     (memo-dfs graph (third parts) memo)))
            ((string= (second parts) "OR")
             (logior (memo-dfs graph (first parts) memo)
                     (memo-dfs graph (third parts) memo)))
            ((string= (second parts) "LSHIFT")
             (ash (memo-dfs graph (first parts) memo)
                  (memo-dfs graph (third parts) memo)))
            ((string= (second parts) "RSHIFT")
             (ash (memo-dfs graph (first parts) memo)
                  (- (memo-dfs graph (third parts) memo))))
            (t (error "Unknown rule or malformed input: ~a" source-rule))))

    (setf (gethash entry memo) result)
    result))

(defun some-assembly-required (input-string)
  (let ((wire-to-rule (make-hash-table :test 'equal)))
    (loop for inst in (split-string-by-char input-string #\Newline)
          do (let ((parts (split-string-by-substring inst " -> ")))
               (when (= (length parts) 2)
                 (setf (gethash (second parts) wire-to-rule) (first parts)))))

    (let* ((memo1 (make-hash-table :test 'equal))
           (a-signal (memo-dfs wire-to-rule "a" memo1)))

      (let ((memo2 (make-hash-table :test 'equal)))
        (setf (gethash "b" wire-to-rule) (format nil "~a" a-signal))
        (memo-dfs wire-to-rule "a" memo2)))))

(defun main ()
  (let ((input-data (read-file-content "input.txt")))
    (print (some-assembly-required input-data))))

(main)
