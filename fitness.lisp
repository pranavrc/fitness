;; Genetic programming framework using Common Lisp.
;; Pranav Ravichandran <me@onloop.net>

(defun get-arity (primitive-alist func)
  (cadr (assoc func primitive-alist)))

(defun pick-random (inlist)
  (nth (random (length inlist)) inlist))

(defun generate-program-tree (primitive-alist operator-list max-tree-depth)
  (let ((operator (pick-random (append primitive-alist operator-list))))
    (if (<= max-tree-depth 0)
      (car (pick-random operator-list))
      (append (list (car operator))
              (loop for args upto (- (cadr operator) 1)
                    collect (generate-program-tree
                              primitive-alist
                              operator-list
                              (- max-tree-depth 1)))))))

(defun generate-population (primitives operators max-tree-depth &optional (member-count 1000))
  (loop for args upto (- member-count 1)
        collect (generate-program-tree primitives operators max-tree-depth)))

(defmacro evaluate-tree (tree args repeat)
  (if (> repeat 0)
    (macrolet ((program () `,(list 'lambda '(&rest args) tree)))
      (let ((result (funcall (program) args)))
        (cons result (evaluate-tree tree result (- repeat 1)))))
    nil))

(defun evaluate-population (population args repeat &optional (repeat 50))
  (mapcar #'(lambda (tree) (cons tree (evaluate-tree tree args repeat))) population))

(defun generation-fitness (population results fitness-function)
  (pairlis population
           (mapcar #'(lambda (result) (fitness-function result)) results)))

(defun get-min-max (alist predicate key)
  (when list
    (let* ((m0 (first list))
           (m1 (funcall key m0)))
      (mapc (lambda (e0 &aux (e1 (funcall key e0)))
              (when (funcall predicate e1 m1)
                (psetf m0 e0 m1 e1)))
            list)
      m0)))

(defun tournament-selection (fitness-alist selection-count random-count)
  (loop for count upto selection-count collect
        (get-min-max (loop for rand-count upto random-count collect
                           pick-random fitness-alist) #'> #'second)))

(defun fair-coin ()
  (let ((toss (random 101)))
    (cond ((< toss 50) t)
          ((> toss 50) nil)
          (t 'edge))))

(defun pick-random-subtree (tree &optional (acc '()))
  (if (fair-coin)
    (let* ((pick-number (random (length (cdr tree))))
           (pick (nth (+ 1 pick) tree))
           (append acc (list pick-number)))
      (pick-random-subtree pick acc))
    (list :acc acc :subtree tree)))

(defun crossover (mother father)
  (let ((mother-st (pick-random-subtree mother))
        (father-st (pick-random-subtree father)))))

