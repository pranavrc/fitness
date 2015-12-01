;; Genetic programming framework using Common Lisp.
;; Pranav Ravichandran <me@onloop.net>

(defun get-arity (primitive-alist func)
  (cadr (assoc func primitive-alist)))

(defun pick-random (inlist)
  (nth (random (length inlist)) inlist))

(defun generate-program-tree (primitive-alist action-list conditional-list max-tree-depth)
  (let* ((toss (fair-coin 50))
         (toss2 (fair-coin 50))
         (pick (if toss
                 (if toss2
                   (pick-random conditional-list)
                   (pick-random action-list))
                 (pick-random primitive-alist))))
    (if toss
      pick
      (if (<= max-tree-depth 0)
        (pick-random conditional-list)
        (append (list (car pick))
                (loop for args upto (- (cadr pick) 1)
                      collect (generate-program-tree
                                primitive-alist
                                action-list
                                conditional-list
                                (- max-tree-depth 1))))))))

(defun generate-population (primitives actions conditionals max-tree-depth &optional (member-count 1000))
  (loop for args upto (- member-count 1)
        collect (generate-program-tree primitives actions conditionals max-tree-depth)))

(defun evaluate-tree (tree args repeat-var)
  (if (> repeat-var 0)
    (macrolet ((program (code) `(eval (list 'lambda args ,code))))
      (let ((result (funcall (program tree) args)))
        (cons result (evaluate-tree tree result (- repeat-var 1)))))
    nil))

(defun evaluate-population (population args repeat-var)
  (mapcar #'(lambda (tree) (cons tree (evaluate-tree tree args repeat-var))) population))

(defun generation-fitness (population args fitness-function &optional (repeat-var 50))
  (pairlis population
           (mapcar #'(lambda (result) (fitness-function result))
                   (evaluate-population population args repeat-var))))

(defun get-min-max (alist predicate key)
  (when list
    (let* ((m0 (first list))
           (m1 (funcall key m0)))
      (mapc (lambda (e0 &aux (e1 (funcall key e0)))
              (when (funcall predicate e1 m1)
                (psetf m0 e0 m1 e1)))
            list)
      m0)))

(defun tournament-selection (fitness-alist program-count)
  (mapcar #'car (get-min-max (loop for r-count upto program-count collect
                                   (pick-random fitness-alist)) #'> #'second)))

(defun fair-coin (chance)
  (let ((toss (random 101)))
    (cond ((< toss chance) t)
          ((> toss chance) nil)
          (t 'edge))))

(defun is-tree (tree)
  (every #'(lambda (el) (typep el 'list)) tree))

(defun pick-random-subtree (tree
                             &optional (replacement nil) (orig tree) (acc '()))
  (let ((branches (cdr tree)))
    (if (is-tree branches)
      (let* ((pick-number (random (length branches)))
             (acc (append acc (list pick-number)))
             (pick (nth pick-number branches)))
        (if (fair-coin 75)
          (pick-random-subtree pick replacement orig acc)
          (if replacement
            (replace-subtree orig nth-list replacement)
            pick)))
      (if replacement
        (replace-subtree orig nth-list replacement)
        tree))))

(defun recursive-nth (tree nth-list)
  (let ((target (nth (car nth-list) tree)))
    (if target
      (if (<= (length nth-list) 1)
        (list 'nth (car nth-list) (quote tree))
        (list 'nth (car nth-list)
              (recursive-nth tree (cdr nth-list))))
      (quote tree))))

(defun replace-subtree (tree nth-list replacement)
  (macrolet
    ((macro-nth (atree list-nth)
                `(eval (list 'lambda '(tree replacement)
                             (list 'setf (recursive-nth ,atree ,list-nth) 'replacement)))))
    (let ((setf-return (funcall (macro-nth tree nth-list) tree replacement)))
      tree)))

(defun crossover (mother father)
  (let ((father-st (pick-random-subtree father))
        (mother-st (pick-random-subtree mother father)))
    mother-st))

(defun copy-population (fitness-alist &optional (percentage 10) (program-count 7))
  (loop for s-count upto (* (length fitness-alist) (/ percentage 100)) collect
        (tournament-selection fitness-alist program-count)))

(defun crossover-population (fitness-alist &optional (percentage 10) (program-count 7))
  (loop for s-count upto (* (length fitness-alist) (/ percentage 100)) collect
        (let ((mother (tournament-selection fitness-alist program-count))
              (father (tournament-selection fitness-alist program-count)))
          (crossover mother father))))

(defun evolve (primitives actions conditionals args fitness-function fitness-p &optional
                          (max-tree-depth 5) (member-count 1000) (repeat-var 10)
                          (percentage 10) (program-count 7) (generation nil))
  (let* ((population
           (if generation
             generation
             (generate-population primitives actions conditionals max-tree-depth member-count)))
         (population-fitness (generation-fitness population args fitness-function repeat-var))
         (passes (remove-if-not #'(lambda (x) (fitness-p (cadr x))) population-fitness)))
    (if passes
      passes
      (evolve primitives actions conditionals args fitness-function fitness-p
              max-tree-depth member-count repeat-var percentage program-count
              (append (copy-population population-fitness percentage program-count)
              (crossover-population population-fitness percentage program-count))))))

