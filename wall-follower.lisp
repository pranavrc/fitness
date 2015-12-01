(defun make-grid-world (dimensions)
  (make-array dimensions :initial-element 0))

(defun current-cell (x y) (cons x y))

(defun n (grid-world current-cell)
  (let ((new-x (- (car current-cell) 1)))
    (list grid-world
          (if (>= new-x 0)
            (cons new-x (cdr current-cell))
            current-cell))))

(defun s (grid-world current-cell)
  (let ((new-x (+ (car current-cell) 1)))
    (list grid-world
          (if (<= new-x (- (car (array-dimensions grid-world)) 1))
            (cons new-x (cdr current-cell))
            current-cell))))

(defun e (grid-world current-cell)
  (let ((new-y (+ (cdr current-cell) 1)))
    (list grid-world
          (if (<= new-y (- (cadr (array-dimensions grid-world)) 1))
            (cons (car current-cell) new-y)
            current-cell))))

(defun w (grid-world current-cell)
  (let ((new-y (- (cdr current-cell) 1)))
    (list grid-world
          (if (>= new-y 0)
            (cons (car current-cell) new-y)
            current-cell))))

(defun ne (grid-world current-cell)
  (let ((new-x (- (car current-cell) 1))
        (new-y (+ (cdr current-cell) 1)))
    (list grid-world
          (if (and (>= new-x 0)
                   (<= new-y (- (cadr (array-dimensions grid-world)) 1)))
            (cons new-x new-y)
            current-cell))))

(defun nw (grid-world current-cell)
  (let ((new-x (- (car current-cell) 1))
        (new-y (- (cdr current-cell) 1)))
    (list grid-world
          (if (and (>= new-x 0) (>= new-y 0))
            (cons new-x new-y)
            current-cell))))

(defun se (grid-world current-cell)
  (let ((new-x (+ (car current-cell) 1))
        (new-y (+ (cdr current-cell) 1)))
    (list grid-world
          (if (and (<= new-x (- (car (array-dimensions grid-world)) 1))
                   (<= new-y (- (cadr (array-dimensions grid-world)) 1)))
            (cons new-x new-y)
            current-cell))))

(defun sw (grid-world current-cell)
  (let ((new-x (+ (car current-cell) 1))
        (new-y (- (cdr current-cell) 1)))
    (list grid-world
          (if (and (<= new-x (- (car (array-dimensions grid-world)) 1))
                   (>= new-y 0))
            (cons new-x new-y)
            current-cell))))

(defun n-sensor (grid-world current-cell)
  (= (car current-cell) 0))

(defun s-sensor (grid-world current-cell)
  (= (car current-cell) (- (car (array-dimensions grid-world)) 1)))

(defun e-sensor (grid-world current-cell)
  (= (cdr current-cell) (- (cadr (array-dimensions grid-world)) 1)))

(defun w-sensor (grid-world current-cell)
  (= (cdr current-cell) 0))

(defun ne-sensor (grid-world current-cell)
  (and (n-sensor grid-world current-cell) (e-sensor grid-world current-cell)))

(defun nw-sensor (grid-world current-cell)
  (and (n-sensor grid-world current-cell) (w-sensor grid-world current-cell)))

(defun se-sensor (grid-world current-cell)
  (and (s-sensor grid-world current-cell) (e-sensor grid-world current-cell)))

(defun sw-sensor (grid-world current-cell)
  (and (s-sensor grid-world current-cell) (w-sensor grid-world current-cell)))

(setf primitives '((if 3) (and 2) (or 2) (not 1)))

(setf actions '((n grid-world current-cell)
                (w grid-world current-cell)
                (e grid-world current-cell)
                (s grid-world current-cell)))
;               (ne grid-world current-cell)
;               (nw grid-world current-cell)
;               (se grid-world current-cell)
;               (sw grid-world current-cell)))

(setf conditionals '((n-sensor grid-world current-cell)
                     (w-sensor grid-world current-cell)
                     (e-sensor grid-world current-cell)
                     (s-sensor grid-world current-cell)
                     (ne-sensor grid-world current-cell)
                     (nw-sensor grid-world current-cell)
                     (se-sensor grid-world current-cell)
                     (sw-sensor grid-world current-cell)))

(defun wall-cell-p (grid-world cell)
  (let ((dimensions (array-dimensions grid-world)))
    (or (= (car cell) 0)
        (= (cdr cell) 0)
        (= (car cell) (- (car dimensions) 1))
        (= (cdr cell) (- (cadr dimensions) 1)))))

(defun new-cell-p (grid-world cell)
  (not (= (aref grid-world (car cell) (cdr cell)) 1)))

(defun fitness-p (grid-world fitness)
  (let* ((dimensions (array-dimensions grid-world))
         (wall-cells (+ (* 2 (car dimensions)) (* 2 (- 2 (cadr dimensions))))))
    (>= fitness wall-cells)))

(defun fitness-function (results)
  (loop for result in results
        counting #'(lambda (result)
                     (if (and (new-cell-p (car result) (cadr result))
                              (wall-cell-p (car result) (cadr result)))))
        into fitness
        finally (return fitness)))

