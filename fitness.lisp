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
  (mapcar #'(lambda (tree) (evaluate-tree tree args repeat)) population))

(defun generation-fitness (population results fitness-function)
  (pairlis population
           (mapcar #'(lambda (result) (fitness-function result)) results)))
