
(ql:quickload :cl-ppcre)
(ql:quickload :uiop)

(defpackage :passport-validator
  (:use :cl :cl-ppcre)
  (:export :main))

(in-package :passport-validator)

(defparameter *required-fields* '(:byr :iyr :eyr :hgt :hcl :ecl :pid))

(defun safe-parse-integer (s)
  (when (scan "^\\d+$" s)
    (handler-case (parse-integer s)
      (error () nil))))

(defun is-valid-year (year-str min max)
  (let ((num (safe-parse-integer year-str)))
    (and num
         (= (length year-str) 4)
         (>= num min)
         (<= num max))))

(defun is-valid-height (height-str)
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan-to-strings "^(\\d+)(cm|in)$" height-str)
    (declare (ignore match-start match-end reg-ends))
    (when (and reg-starts (plusp (length reg-starts)))
      (let* ((value (safe-parse-integer (aref reg-starts 0)))
             (unit (aref reg-starts 1)))
        (and value
             (cond
               ((string= unit "cm") (and (>= value 150) (<= value 193)))
               ((string= unit "in") (and (>= value 59) (<= value 76)))
               (t nil)))))))

(defun is-valid-hair-color (color-str)
  (scan "^#[0-9a-f]{6}$" color-str))

(defun is-valid-eye-color (color-str)
  (member color-str '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test 'string=))

(defun is-valid-passport-id (id-str)
  (scan "^\\d{9}$" id-str))

(defun is-valid-passport (passport-alist)
  (every (lambda (field-key)
           (let ((value (getf passport-alist field-key)))
             (cond
               ((null value) nil)
               ((eq field-key :byr) (is-valid-year value 1920 2002))
               ((eq field-key :iyr) (is-valid-year value 2010 2020))
               ((eq field-key :eyr) (is-valid-year value 2020 2030))
               ((eq field-key :hgt) (is-valid-height value))
               ((eq field-key :hcl) (is-valid-hair-color value))
               ((eq field-key :ecl) (is-valid-eye-color value))
               ((eq field-key :pid) (is-valid-passport-id value))
               (t t))))
         *required-fields*))

(defun parse-passport-block (block-str)
  (let ((passport-alist '()))
    (dolist (field-str (split "\\s+" block-str))
      (when (plusp (length field-str))
        (let ((parts (split ":" field-str)))
          (when (= (length parts) 2)
            (setf passport-alist (acons (intern (string-upcase (first parts)) :keyword)
                                        (second parts)
                                        passport-alist))))))
    passport-alist))

(defun parse-passports (data-str)
  (mapcar #'parse-passport-block (split "\\n\\n" data-str)))

(defun main ()
  (let* ((data (uiop:read-file-string "input.txt"))
         (passports (parse-passports data))
         (valid-count (count-if #'is-valid-passport passports)))
    (format t "~a~%" valid-count)))

(main)
