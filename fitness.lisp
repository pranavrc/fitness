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

(defun evaluate-population (population)
  (mapcar #'eval population)

(defun generate-population (primitives operators max-tree-depth &optional (member-count 1000))
  (loop for args upto (- member-count 1)
        collect (generate-program-tree primitives operators max-tree-depth)))
