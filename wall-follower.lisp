(defun make-grid-world (dimensions)
  (make-array dimensions :initial-element 0))

(defun current-cell (x y) (cons x y))

(defun n (grid-world current-cell)
  (let ((new-x (- (car current-cell) 1)))
    (if (>= new-x 0)
      (cons new-x (cdr current-cell))
      current-cell)))

(defun s (grid-world current-cell)
  (let ((new-x (+ (car current-cell) 1)))
    (if (<= new-x (- (car (array-dimensions grid-world)) 1))
      (cons new-x (cdr current-cell))
      current-cell)))

(defun e (grid-world current-cell)
  (let ((new-y (+ (cdr current-cell) 1)))
    (if (<= new-y (- (cadr (array-dimensions grid-world)) 1))
      (cons (car current-cell) new-y)
      current-cell)))

(defun w (grid-world current-cell)
  (let ((new-y (- (cdr current-cell) 1)))
    (if (>= new-y 0)
      (cons (car current-cell) new-y)
      current-cell)))

(defun ne (grid-world current-cell)
  (let ((new-x (- (car current-cell) 1))
        (new-y (+ (cdr current-cell) 1)))
    (if (and (>= new-x 0)
             (<= new-y (- (cadr (array-dimensions grid-world)) 1)))
      (cons new-x new-y)
      current-cell)))

(defun nw (grid-world current-cell)
  (let ((new-x (- (car current-cell) 1))
        (new-y (- (cdr current-cell) 1)))
    (if (and (>= new-x 0) (>= new-y 0))
      (cons new-x new-y)
      current-cell)))

(defun se (grid-world current-cell)
  (let ((new-x (+ (car current-cell) 1))
        (new-y (+ (cdr current-cell) 1)))
    (if (and (<= new-x (- (car (array-dimensions grid-world)) 1))
             (<= new-y (- (cadr (array-dimensions grid-world)) 1)))
      (cons new-x new-y)
      current-cell)))

(defun sw (grid-world current-cell)
  (let ((new-x (+ (car current-cell) 1))
        (new-y (- (cdr current-cell) 1)))
    (if (and (<= new-x (- (car (array-dimensions grid-world)) 1))
             (>= new-y 0))
      (cons new-x new-y)
      current-cell)))

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

