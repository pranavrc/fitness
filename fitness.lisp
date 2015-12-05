;; Genetic programming framework using Common Lisp.
;; Pranav Ravichandran <me@onloop.net>

(defun get-arity (primitive-alist func)
  "When passed an assoc list of function-arity pairs,
  return the arity of a specific function."
  (cadr (assoc func primitive-alist)))

(defun pick-random (inlist)
  "Pick a random element from a list."
  (nth (random (length inlist)) inlist))

(defun generate-program-tree (primitive-alist action-list conditional-list max-tree-depth)
  "Generate a random program from a list of primitives, actions
  and conditionals. The maximum depth of the program tree will be max-tree-depth."
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

(defun generate-population (primitives actions conditionals max-tree-depth member-count)
  "Create a generation of random programs with population member-count."
  (loop for args upto (- member-count 1)
        collect (generate-program-tree primitives actions conditionals max-tree-depth)))

(defun evaluate-tree (tree repeat-var args fargs)
  "Run a program with args as the name of the arguments
  and fargs being the evaluated result of args. Runs the program repeat-var times,
  each time feeding the output back to the input."
  (if (> repeat-var 0)
    (macrolet ((program (code) `(eval (list 'lambda args ,code))))
      (let ((result (apply (program tree) fargs)))
        (if (and result (equal (type-of result) (type-of fargs)))
          (cons result (evaluate-tree tree (- repeat-var 1) args result))
          (list fargs))))
    nil))

(defun evaluate-population (population repeat-var args fargs)
  "Evaluate a generation of programs."
  (mapcar #'(lambda (tree)
              (list tree
                    (mapcar #'(lambda (farg)
                                (evaluate-tree tree repeat-var args farg))
                            fargs))) population))

(defun generation-fitness (population args fargs fitness-function &optional (repeat-var 50))
  "Evaluate the fitness of a generation of programs using provided fitness-function."
  (pairlis population
           (mapcar #'(lambda (result) (fitness-function (cadr result)))
                   (evaluate-population population repeat-var args fargs))))

(defun get-min-max (alist predicate key)
  "Depending on the predicate arg, get the min or max element
  from an association list by value."
  (when alist
    (let* ((m0 (first alist))
           (m1 (funcall key m0)))
      (mapc (lambda (e0 &aux (e1 (funcall key e0)))
              (when (funcall predicate e1 m1)
                (psetf m0 e0 m1 e1)))
            alist)
      m0)))

(defun get-max-assoc (fitness-alist)
  "Get key-value pair with max value in association list."
  (loop for i in fitness-alist
        maximizing (cdr i) into max
        finally (return (rassoc max fitness-alist))))

(defun tournament-selection (fitness-alist program-count)
  "Pick a bunch of programs from a generation, choose the fittest
  one and return it. The number of programs picked can be modified
  with program-count."
  (car (get-max-assoc (loop for r-count upto (- program-count 1) collect
                            (pick-random fitness-alist)))))

(defun fair-coin (chance)
  "Fair coin toss whose probability of heads can be modified with
  the chance argument."
  (let ((toss (random 101)))
    (cond ((< toss chance) t)
          ((> toss chance) nil)
          (t 'edge))))

(defun is-tree (tree)
  "Checks if a list is a tree or a flat list."
  (every #'(lambda (el) (typep el 'list)) tree))

(defun pick-random-subtree (tree
                             &optional (replacement nil) (acc '()))
  "Picks a random subtree from a program tree. If
  replacement is provided, calls a routine to replace the subtree."
  (let ((branches (cdr tree)))
    (if (is-tree branches)
      (let* ((pick-number (random (length branches)))
             (acc (append acc (list (+ 1 pick-number))))
             (pick (nth pick-number branches)))
        (if (fair-coin 75)
          (pick-random-subtree pick replacement acc)
          (if replacement acc pick)))
      (if replacement acc tree))))

(defun recursive-nth (tree nth-list)
  "Generates (nth (nth (nth ...))) trees for
  destructive replacement of a nested element
  within a tree."
  (let ((target (nth (car nth-list) tree)))
    (if target
      (if (<= (length nth-list) 1)
        (list 'nth (car nth-list) (quote tree))
        (list 'nth (car nth-list)
              (recursive-nth tree (cdr nth-list))))
      (quote tree))))

(defun replace-subtree (tree nth-list replacement)
  "Replaces one subtree with another, using the
  generated nth-list."
  (if (and (is-tree (cdr tree)) nth-list replacement)
    (macrolet
      ((macro-nth (atree list-nth)
                  `(eval (list 'lambda '(tree replacement)
                               (list 'setf (recursive-nth ,atree ,list-nth) 'replacement)))))
      (let ((setf-return (funcall (macro-nth tree (reverse nth-list)) tree replacement)))
        tree))
    tree))

(defun crossover (mother father)
  "Picks a random subtree from the father and the mother,
  and replaces the subtree of the mother with the subtree
  of the father."
  (let* ((father-st (pick-random-subtree father))
         (mother-st-acc (pick-random-subtree mother father-st))
         (mother-st (replace-subtree mother mother-st-acc father-st)))
    mother-st))

(defun copy-population (fitness-alist &optional (percentage 10) (program-count 7))
  "Copies a set of programs directly from one generation to another."
  (loop for s-count upto (* (length fitness-alist) (/ percentage 100)) collect
        (tournament-selection fitness-alist program-count)))

(defun crossover-population (fitness-alist &optional (percentage 90) (program-count 7))
  "Copies a set of programs from one generation to another using crossovers."
  (loop for s-count upto (* (length fitness-alist) (/ percentage 100)) collect
        (let ((mother (tournament-selection fitness-alist program-count))
              (father (tournament-selection fitness-alist program-count)))
          (crossover mother father))))

(defun evolve (primitives actions conditionals args fargs fitness-function fitness-p
                          &optional
                          (max-run-count 10) (max-tree-depth 5) (member-count 1000) (repeat-var 60)
                          (copy-percentage 10) (crossover-percentage 90) (program-count 7)
                          (generation nil))
  "Main function to run the evolution process.

  Mandatory arguments -

  primitives - Primitive functions to be used (eg., if, and, or and not).
  actions - User defined functions that operate on the user-defined world.
  conditionals - User defined 'sensors' that operate on the user-defined world.
  args - A raw list of the arguments that the 'action' functions can take.
  fargs - An evaluted list of arguments that the 'action' functions can take.
  fitness-function - The fitness function that takes a set of results and returns a fitness.
  fitness-p - Predicate function that determines if a fitness has reached 'success' state.
  max-tree-depth - Maximum allowed depth of program trees that need to be generated.

  Optional arguments -

  max-run-count - Maximum number of evolutions to be run.
  max-tree-depth - Maximum depth of program trees generated. Default 5.
  member-count - Number of programs in each generation. Default 1000.
  repeat-var - Number of evaluations needed on each program. Default 60.
  copy-percentage - Percentage of programs to evolve by direct copying.
  crossover-percentage - Percentage of programs to evolve by crossing over.
  program-count - Number of programs to be selected on each round of tournament selection."
  (let* ((population
           (if generation
             generation
             (generate-population primitives actions conditionals max-tree-depth member-count)))
         (population-fitness (generation-fitness population args fargs fitness-function repeat-var))
         (passes (remove-if-not #'(lambda (x) (fitness-p (cdr x))) population-fitness)))
    (if (> max-run-count 0)
      (if passes
        passes
        (evolve primitives actions conditionals args fargs fitness-function fitness-p
                (- max-run-count 1)
                max-tree-depth member-count repeat-var
                copy-percentage crossover-percentage program-count
                (append
                  (copy-population population-fitness copy-percentage program-count)
                  (crossover-population population-fitness crossover-percentage program-count))))
      population-fitness)))

