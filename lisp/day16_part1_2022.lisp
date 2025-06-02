
(defun get-input-file-content (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let* ((size (file-length stream))
           (content (make-string size)))
      (read-sequence content stream)
      content)))

(defun split-string-by-substring (s sub)
  (loop with len-sub = (length sub)
        for i = 0 then (+ j len-sub)
        for j = (search sub s :start2 i)
        collect (subseq s i j)
        while j))

(defun parse-line (line)
  (let* ((parts (split-string-by-substring line "; "))
         (id-flow-part (first parts))
         (tunnels-part (second parts))

         (id-str (second (split-string-by-substring id-flow-part " ")))
         (flow-str (second (split-string-by-substring id-flow-part "=")))
         (flow (parse-integer flow-str))

         (tunnels-list-raw (subseq tunnels-part (length "tunnel leads to valve")))
         (actual-tunnels-list-str (if (string= (subseq tunnels-list-raw 0 1) "s")
                                      (subseq tunnels-list-raw 2)
                                      (subseq tunnels-list-raw 1))))

    (let ((valve-data (make-hash-table :test 'equal)))
      (setf (gethash "id" valve-data) id-str)
      (setf (gethash "flow" valve-data) flow)
      (let ((tunnels-map (make-hash-table :test 'equal)))
        (setf (gethash id-str tunnels-map) 0)
        (dolist (tunnel (split-string-by-substring actual-tunnels-list-str ", "))
          (setf (gethash tunnel tunnels-map) 1))
        (setf (gethash "tunnels" valve-data) tunnels-map))
      valve-data)))

(defvar *valves* (make-hash-table :test 'equal))

(defun calculate-all-pairs-shortest-paths ()
  (loop for k being the hash-key in *valves* do
    (loop for i being the hash-key in *valves* do
      (loop for j being the hash-key in *valves* do
        (let* ((i-valve (gethash i *valves*))
               (k-valve (gethash k *valves*))
               (i-tunnels (gethash "tunnels" i-valve))
               (k-tunnels (gethash "tunnels" k-valve)))

          (multiple-value-bind (dik okik) (gethash k i-tunnels)
            (multiple-value-bind (dkj okkj) (gethash j k-tunnels)
              (when (and okik okkj)
                (multiple-value-bind (dij okij) (gethash j i-tunnels)
                  (when (or (not okij) (> dij (+ dik dkj)))
                    (setf (gethash j i-tunnels) (+ dik dkj))))))))))))

(defun max-pressure (curr minute pressure open-valves)
  (let ((max-val pressure))
    (loop for next-valve in open-valves
          do (let* ((time-to-move (gethash next-valve (gethash "tunnels" (gethash curr *valves*))))
                    (time-left (- minute time-to-move 1)))
               (when (> time-left 0)
                 (let* ((flow-rate (gethash "flow" (gethash next-valve *valves*)))
                        (new-pressure (+ pressure (* time-left flow-rate)))
                        (new-open-valves (remove next-valve open-valves :test 'equal)))
                   (setf max-val (max max-val (max-pressure next-valve time-left new-pressure new-open-valves)))))))
    max-val))

(defun main ()
  (let ((input-content (get-input-file-content "input.txt")))
    (dolist (line (split-string-by-substring input-content #.(string #\Newline)))
      (unless (string= line "")
        (let ((parsed-valve (parse-line line)))
          (setf (gethash (gethash "id" parsed-valve) *valves*) parsed-valve)))))

  (calculate-all-pairs-shortest-paths)

  (let ((initial-open-valves '()))
    (loop for valve-id being the hash-key in *valves*
          when (> (gethash "flow" (gethash valve-id *valves*)) 0)
          do (push valve-id initial-open-valves))

    (format t "~a~%" (max-pressure "AA" 30 0 initial-open-valves))))

(main)
