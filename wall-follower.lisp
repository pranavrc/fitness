;; Wall-follower robot as an extension to the gp framework.
;; Pranav Ravichandran <me@onloop.net>

(defun make-grid-world (dimensions)
  "Make a matrix with specific dimensions."
  (make-array dimensions :initial-element 0))

(defun current-cell (x y)
  "Make a cons cell to denote the current coordinates
  of the wall-following robot on the grid world."
  (cons x y))

(defun n (grid-world current-cell)
  "Move the robot north by one cell in the grid world."
  (let ((new-x (- (car current-cell) 1)))
    (list grid-world
          (if (>= new-x 0)
            (cons new-x (cdr current-cell))
            current-cell))))

(defun s (grid-world current-cell)
  "Move the robot south by one cell in the grid world."
  (let ((new-x (+ (car current-cell) 1)))
    (list grid-world
          (if (<= new-x (- (car (array-dimensions grid-world)) 1))
            (cons new-x (cdr current-cell))
            current-cell))))

(defun e (grid-world current-cell)
  "Move the robot east by one cell in the grid world."
  (let ((new-y (+ (cdr current-cell) 1)))
    (list grid-world
          (if (<= new-y (- (cadr (array-dimensions grid-world)) 1))
            (cons (car current-cell) new-y)
            current-cell))))

(defun w (grid-world current-cell)
  "Move the robot west by one cell in the grid world."
  (let ((new-y (- (cdr current-cell) 1)))
    (list grid-world
          (if (>= new-y 0)
            (cons (car current-cell) new-y)
            current-cell))))

(defun ne (grid-world current-cell)
  "Move the robot north east by one cell in the grid world."
  (let ((new-x (- (car current-cell) 1))
        (new-y (+ (cdr current-cell) 1)))
    (list grid-world
          (if (and (>= new-x 0)
                   (<= new-y (- (cadr (array-dimensions grid-world)) 1)))
            (cons new-x new-y)
            current-cell))))

(defun nw (grid-world current-cell)
  "Move the robot north west by one cell in the grid world."
  (let ((new-x (- (car current-cell) 1))
        (new-y (- (cdr current-cell) 1)))
    (list grid-world
          (if (and (>= new-x 0) (>= new-y 0))
            (cons new-x new-y)
            current-cell))))

(defun se (grid-world current-cell)
  "Move the robot south east by one cell in the grid world."
  (let ((new-x (+ (car current-cell) 1))
        (new-y (+ (cdr current-cell) 1)))
    (list grid-world
          (if (and (<= new-x (- (car (array-dimensions grid-world)) 1))
                   (<= new-y (- (cadr (array-dimensions grid-world)) 1)))
            (cons new-x new-y)
            current-cell))))

(defun sw (grid-world current-cell)
  "Move the robot south west by one cell in the grid world."
  (let ((new-x (+ (car current-cell) 1))
        (new-y (- (cdr current-cell) 1)))
    (list grid-world
          (if (and (<= new-x (- (car (array-dimensions grid-world)) 1))
                   (>= new-y 0))
            (cons new-x new-y)
            current-cell))))

(defun n-sensor (grid-world current-cell)
  "Check if there's a wall north of the robot."
  (= (car current-cell) 0))

(defun s-sensor (grid-world current-cell)
  "Check if there's a wall south of the robot."
  (= (car current-cell) (- (car (array-dimensions grid-world)) 1)))

(defun e-sensor (grid-world current-cell)
  "Check if there's a wall east of the robot."
  (= (cdr current-cell) (- (cadr (array-dimensions grid-world)) 1)))

(defun w-sensor (grid-world current-cell)
  "Check if there's a wall west of the robot."
  (= (cdr current-cell) 0))

(defun ne-sensor (grid-world current-cell)
  "Check if there's a wall north east of the robot."
  (and (n-sensor grid-world current-cell) (e-sensor grid-world current-cell)))

(defun nw-sensor (grid-world current-cell)
  "Check if there's a wall north west of the robot."
  (and (n-sensor grid-world current-cell) (w-sensor grid-world current-cell)))

(defun se-sensor (grid-world current-cell)
  "Check if there's a wall south east of the robot."
  (and (s-sensor grid-world current-cell) (e-sensor grid-world current-cell)))

(defun sw-sensor (grid-world current-cell)
  "Check if there's a wall south west of the robot."
  (and (s-sensor grid-world current-cell) (w-sensor grid-world current-cell)))

;; For the 5x5 grid world.
(setf dims '(5 5))

;; The 5x5 grid world.
(setf grid-world (make-grid-world dims))

;; List of starting positions.
(setf current-cell (list (current-cell 2 2)
                         (current-cell 0 0)
                         (current-cell 4 4)
                         (current-cell 0 4)
                         (current-cell 4 0)
                         (current-cell 1 1)
                         (current-cell 3 3)
                         (current-cell 1 3)
                         (current-cell 3 1)
                         (current-cell 0 3)))

;; Assoc list of primitives and their arity.
(setf primitives '((if 3) (and 2) (or 2) (not 1)))

;; Args for the lambda function that generates the program.
(setf args '(grid-world current-cell))

;; Value of args for the generated program.
(setf fargs (mapcar #'(lambda (x) (list grid-world x)) current-cell))

;; Set of actions.
(setf actions '((n grid-world current-cell)
                (w grid-world current-cell)
                (e grid-world current-cell)
                (s grid-world current-cell)))
;               (ne grid-world current-cell)
;               (nw grid-world current-cell)
;               (se grid-world current-cell)
;               (sw grid-world current-cell)))

;; Set of conditionals/sensor programs.
(setf conditionals '((n-sensor grid-world current-cell)
                     (w-sensor grid-world current-cell)
                     (e-sensor grid-world current-cell)
                     (s-sensor grid-world current-cell)
                     (ne-sensor grid-world current-cell)
                     (nw-sensor grid-world current-cell)
                     (se-sensor grid-world current-cell)
                     (sw-sensor grid-world current-cell)))

;; Check if a cell in the grid world is a wall cell.
(defun wall-cell-p (grid-world cell)
  (let ((dimensions (array-dimensions grid-world)))
    (or (= (car cell) 0)
        (= (cdr cell) 0)
        (= (car cell) (- (car dimensions) 1))
        (= (cdr cell) (- (cadr dimensions) 1)))))

;; Check if in a single evaluation the cell has not already been visited.
(defun new-cell-p (hashset cell)
  (not (gethash (write-to-string cell) hashset)))

;; FItness predicate function that checks if a fitness has 'succeeded'.
(defun fitness-p (fitness)
  (let* ((dimensions dims)
         (wall-cells (+ (* 2 (car dimensions)) (* 2 (- (cadr dimensions) 2)))))
    (>= fitness (* wall-cells (length current-cell)))))

;; Fitness function helper that takes results and checks them for the fitness.
(defun fitness-helper (results)
  (if (typep results 'list)
    (let ((hashset (make-hash-table :test 'equal)))
      (loop for result in results
            while (new-cell-p hashset (cadr result))
            do (setf (gethash (write-to-string (cadr result)) hashset) t)
            counting #'(lambda (result)
                         (if (and (new-cell-p (car result) (cadr result))
                                  (wall-cell-p (car result) (cadr result)))))
            into fitness
            finally (return fitness)))
    0))

;; Fitness function that maps the fitness helper across the results list.
(defun fitness-function (results)
  (apply #'+ (mapcar #'fitness-helper results)))

