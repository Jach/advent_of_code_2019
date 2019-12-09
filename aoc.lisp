;;;; this file is in the public domain

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cmu-infix)
  (named-readtables:in-readtable cmu-infix:syntax)
  (ql:quickload :alexandria))

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

;; day 3

(defparameter *ex1* "R8,U5,L5,D3
U7,R6,D4,L4")
(defparameter *ex2* "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")

(defun puzzle-to-movements (puzzle)
  (mapcar (lambda (line)
            (let ((moves (uiop:split-string line :separator ",")))
              (mapcar (lambda (move)
                        (cons (elt move 0)
                              (parse-integer (subseq move 1))))
                      moves)))
          (uiop:split-string puzzle :separator (string #\newline))))

;(puzzle-to-movements *ex1*)
; idea: starting at 'origin' (0,0) apply movements and mark hashmap entry as occupied for each step
; then compare the keys of each wire path and find matches
; then distance, min

; no need for hash, just dump moves into a list

(defun move-x (x move)
  (case (car move)
    (#\R (+ x (cdr move)))
    (#\L (- x (cdr move)))
    (t x)))

(defun move-y (y move)
  (case (car move)
    (#\U (+ y (cdr move)))
    (#\D (- y (cdr move)))
    (t y)))

(defun moves-to-occupied-points (moves)
  (let ((points '())
        (x 0)
        (y 0))
    (loop for move in moves do
          (let ((start-x x)
                (start-y y)
                (new-x (move-x x move))
                (new-y (move-y y move)))
            (setf x new-x
                  y new-y)
            (if (< new-x start-x) (rotatef new-x start-x))
            (if (< new-y start-y) (rotatef new-y start-y))
            (loop for step-x from start-x to new-x do
                  (loop for step-y from start-y to new-y do
                        (push (cons step-x step-y) points)))))
    points))

(defun closest-distance (points) ; from origin
  (loop for point in points minimizing (+ (abs (car point))
                                          (abs (cdr point)))))

(defun sol3 (puz)
  (let* ((moves (puzzle-to-movements puz))
         (line1 (first moves))
         (line2 (second moves))
         (points1 (moves-to-occupied-points line1))
         (points2 (moves-to-occupied-points line2)))
    (closest-distance (remove-if (lambda (cell) (equal cell '(0 . 0)))
                                 (intersection points1 points2 :test 'equal)))))
;(sol3 *ex2*)
; so now looking at the actual input, lol, this isn't going to work for such big planes!
; instead lets push moves to a list of line segments, then intersect the line segments.

(defstruct edge
  start-x
  start-y
  end-x
  end-y)
(defun edge (start-x start-y end-x end-y)
  (make-edge :start-x start-x :start-y start-y
             :end-x end-x :end-y end-y))

(defun moves-to-segments (moves)
  (let ((segments '())
        (x 0)
        (y 0))
    (loop for move in moves do
          (let ((new-x (move-x x move))
                (new-y (move-y y move)))
            (push (edge x y new-x new-y) segments)
            (setf x new-x
                  y new-y)))
    segments))

(moves-to-segments (first (puzzle-to-movements *ex1*)))

(defun edge-intersect (edge1 edge2)
  ; make it so we can assume edge1 has same x for start/end, edge2 has same-y for start/end.
  (if (and (= (edge-start-x edge2)
              (edge-end-x edge2))
           (= (edge-start-y edge1)
              (edge-end-y edge1)))
      (rotatef edge1 edge2))
  (if (and (<= (min (edge-start-x edge2) (edge-end-x edge2))
               (edge-start-x edge1)
               (max (edge-start-x edge2) (edge-end-x edge2)))
           (<= (min (edge-start-y edge1) (edge-end-y edge1))
               (edge-start-y edge2)
               (max (edge-start-y edge1) (edge-end-y edge1))))
      (cons (edge-start-x edge1) (edge-start-y edge2))))

;(edge-intersect (edge 0 0 5 0)
;                (edge 1 2 1 -2))

(defun intersects (puz)
  (let* ((moves (puzzle-to-movements puz))
         (line1 (first moves))
         (line2 (second moves))
         (edges1 (moves-to-segments line1))
         (edges2 (moves-to-segments line2)))
    (loop for edge1 in edges1 appending
          (loop for edge2 in edges2
                for intersect = (edge-intersect edge1 edge2)
                if (and intersect (not (equal intersect '(0 . 0))))
                collecting intersect))))

(closest-distance (intersects (uiop:read-file-string #p"day3input")))

; part 2

(defun point-edge (x y)
  (edge x y x y))

(defun point-man-dist (point1 point2)
  (+ (abs (- (car point1) (car point2)))
     (abs (- (cdr point1) (cdr point2)))))

(defun edge-dist (edge) ; dist from start to end
  (point-man-dist (cons (edge-start-x edge)
                        (edge-start-y edge))
                  (cons (edge-end-x edge)
                        (edge-end-y edge))))


(defun steps-to-intersect (wire-moves intersect)
  (let ((ordered-wire-edges (reverse (moves-to-segments wire-moves)))
        (intersect-edge (point-edge (car intersect) (cdr intersect)))
        (steps 0))
    (dolist (edge ordered-wire-edges)
      (alexandria:if-let ((point (edge-intersect edge intersect-edge)))
        (return (incf steps (point-man-dist (cons (edge-start-x edge) (edge-start-y edge))
                                            point)))
        (incf steps (edge-dist edge))))
    steps))

(let* ((puz (uiop:read-file-string #p"day3input"))
       (movements (puzzle-to-movements puz))
       (intersects (intersects puz)))
  (loop for intersect in intersects minimizing
        (+ (steps-to-intersect (first movements) intersect)
           (steps-to-intersect (second movements) intersect))))

;; day 4

(defun meets-rules (num)
  (let ((found-adjacent nil)
        (never-decreases t)
        (prev-char #\null))
    (loop for char across (write-to-string num) do
          (if (eql prev-char char)
              (setf found-adjacent t))
          (if (char> prev-char char)
              (setf never-decreases nil))
          (setf prev-char char))
    (and found-adjacent never-decreases)))

(loop for num from 138307 to 654504 counting (meets-rules num))


(defun meets-rules2 (num)
  (let ((found-adjacent-times 0)
        (never-decreases t)
        (prev-char #\null)
        (count-cons-adjacent 0))
    (loop for char across (write-to-string num) do
          (if (eql prev-char char)
              (progn
                (incf count-cons-adjacent)
                (if (= count-cons-adjacent 1) (incf found-adjacent-times)))
              (progn
                (if (> count-cons-adjacent 1) (decf found-adjacent-times))
                (setf count-cons-adjacent 0)))
          (if (char> prev-char char)
              (setf never-decreases nil))
          (setf prev-char char))
    (if (> count-cons-adjacent 1) (decf found-adjacent-times))
    (and (plusp found-adjacent-times) never-decreases)))

(loop for num from 138307 to 654504 counting (meets-rules2 num))

;; day 5

(defun opcode (instruction)
  "Instruction is a number whose final two digits indicate an opcode."
  (mod instruction 100))

(defun get-operation (instruction)
  "Returns a pair of (args-to-consume function-to-call).
   That is, if the result is (3 lambda), consume the next
   3 items (in whatever mode indicated) and call the lambda
   with the program state followed by the 3 parameters that are aref/set-aref-able.
   The instruction pointer then should advance 4.
   If return is nil, program should halt."
  (case (opcode instruction)
    (1 (list 3 (lambda (p x y store@)
                 (setf (aref p store@) (+ (aref p x)
                                          (aref p y))))))
    (2 (list 3 (lambda (p x y store@)
                 (setf (aref p store@) (* (aref p x)
                                          (aref p y))))))
    (3 (list 1 (lambda (p store@)
                 (setf (aref p store@) (read)))))
    (4 (list 1 (lambda (p out)
                 (print (aref p out)))))
    (99 nil)))

(defun instruction-to-fields (instruction)
  "Returns instruction as a number string
   of 5 digits. Final two digits correspond to the opcode,
   digits above that correspond to parameter modes
   for the params the opcode expects to consume."
  (format nil "~5,'0d" instruction))

(defun read-mode (fields param-num)
  "Given fields (chiefly a string of 3+ chars 0 or 1) and a param-num,
   returns whether the num mapped to a field should be read in
   position or immediate mode.
   i.e. if fields is 010, then (read-mode fields 1) is the right-most 0."
  (if (char-equal (elt fields (- 2 (1- param-num)))
                  #\0)
      :position
      :immediate))

(defun consume-in-mode (p pc consume-count)
  "Given a program, pc index, and consume-count,
   returns a list of length consume-count containing
   positions in p that are ready to aref by a consuming function."
  (let ((fields (instruction-to-fields (aref p pc))))
    (loop for arg-offset from 1 to consume-count collect
          (let ((arg-pos (+ pc arg-offset)))
            (if (eql :immediate (read-mode fields arg-offset))
                arg-pos
                (aref p arg-pos))))))

#|
(consume-in-mode #(1002 4 3 4 33)
                 0
                 3)
(consume-in-mode #(1101 100 -1 4 0)
                 0
                 3)
(consume-in-mode #(4 50) 0 1)
|#


(defun execute-program2 (p)
  "Executes program p, which is an array
   of instructions + memory."
  (setf p (copy-seq p))
  (loop for program-counter below (length p) do
        (let* ((operation (get-operation (aref p program-counter)))
               (consumed-args (first operation))
               (op-func (second operation)))
          ;(format t "PC=~d, instruction=~d, op=~a~%" program-counter (aref p program-counter) operation)
          (if (null op-func) (return))
          (let ((args (consume-in-mode p program-counter consumed-args)))
            (apply op-func p args)
            (incf program-counter (length args)))))
  p)

;(execute-program2 #(1 9 10 3 2 3 11 0 99 30 40 50))
;(execute-program2 #(3 0 4 0 99))
;(execute-program2 #(1002 4 3 4 33))
(let ((p (coerce (mapcar #'parse-integer
                         (uiop:split-string (uiop:read-file-string #p"day5input")
                                            :separator ","))
                 'vector)))
  (execute-program2 p) ; give it value 1 as input for puz
  nil)
