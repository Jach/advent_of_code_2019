;;;; this file is in the public domain

(ql:quickload :cmu-infix)
(named-readtables:in-readtable cmu-infix:syntax)
(ql:quickload :alexandria)

;; day 1

(defun required-fuel (mass)
  #I(floor(mass/3) - 2)) ; learned after, could just be floor(mass, 3) -- floor takes an optional divisor

(defun required-fuel-total (mass)
  (let ((fuel (required-fuel mass)))
    (if (plusp fuel)
        (+ fuel (required-fuel-total fuel))
        (max 0 fuel))))

(defun input-to-required-fuel ()
  (mapcar (lambda (mass) (required-fuel (parse-integer mass)))
          (uiop:read-file-lines #p"day1input")))

(defun input-to-required-fuel-total ()
  (mapcar (lambda (mass) (required-fuel-total (parse-integer mass)))
          (uiop:read-file-lines #p"day1input")))

(defun day1-sol-part1 ()
  (reduce #'+ (input-to-required-fuel)))

(defun day1-sol-part2 ()
  (reduce #'+ (input-to-required-fuel-total)))

;; day 2

(defparameter program (mapcar #'parse-integer (uiop:split-string "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,9,19,23,2,23,13,27,1,27,9,31,2,31,6,35,1,5,35,39,1,10,39,43,2,43,6,47,1,10,47,51,2,6,51,55,1,5,55,59,1,59,9,63,1,13,63,67,2,6,67,71,1,5,71,75,2,6,75,79,2,79,6,83,1,13,83,87,1,9,87,91,1,9,91,95,1,5,95,99,1,5,99,103,2,13,103,107,1,6,107,111,1,9,111,115,2,6,115,119,1,13,119,123,1,123,6,127,1,127,5,131,2,10,131,135,2,135,10,139,1,13,139,143,1,10,143,147,1,2,147,151,1,6,151,0,99,2,14,0,0" :separator ",")))
(defparameter ap (make-array (length program) :initial-contents program))
; better: (coerce program 'vector)
; like
;(defparameter ap (coerce (mapcar #'parse-integer
;                                 (uiop:split-string "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,9,19,23,2,23,13,27,1,27,9,31,2,31,6,35,1,5,35,39,1,10,39,43,2,43,6,47,1,10,47,51,2,6,51,55,1,5,55,59,1,59,9,63,1,13,63,67,2,6,67,71,1,5,71,75,2,6,75,79,2,79,6,83,1,13,83,87,1,9,87,91,1,9,91,95,1,5,95,99,1,5,99,103,2,13,103,107,1,6,107,111,1,9,111,115,2,6,115,119,1,13,119,123,1,123,6,127,1,127,5,131,2,10,131,135,2,135,10,139,1,13,139,143,1,10,143,147,1,2,147,151,1,6,151,0,99,2,14,0,0" :separator ","))
;                         'vector))

(defun execute-program (p)
  (setf p (copy-seq p))
  (loop for opcode-idx below (length p) by 4
        do (let ((opcode (aref p opcode-idx))
                 (op nil))
             (cond ((= opcode 1) (setf op #'+))
                   ((= opcode 2) (setf op #'*))
                   ((= opcode 99) (return)))
             (let ((next (aref p (aref p (1+ opcode-idx))))
                   (next-next (aref p (aref p (+ 2 opcode-idx))))
                   (store (aref p (+ 3 opcode-idx))))
               ;(format t "~a~%" (list op next next-next store))
               (setf (aref p store) (funcall op next next-next)))))
  p)

(defun day2-sol ()
  (setf (aref ap 1) 12
        (aref ap 2) 2)
  (print (aref (execute-program ap) 0))

  (loop for noun from 0 upto 99 do
        (loop for verb from 0 upto 99 do
              (setf (aref ap 1) noun
                    (aref ap 2) verb)
              (when (= (aref (execute-program ap) 0) 19690720)
                (print #I(100 * noun + verb))
                (return)))))
