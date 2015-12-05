# Fitness

Fitness is a Common Lisp genetic programming framework.

### Usage

`evolve` is the central function that initiates and runs the evolution process. It returns the final generation along with the fitness, as the output.

```
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
  program-count - Number of programs to be selected on each round of tournament selection.
```

All these parameters are defined in `wall-follower.lisp`, so `evolve` can simply be run as follows:

```
> (load "wall-follower.lisp")
> (load "fitness.lisp")
> (evolve primitives actions conditionals args fargs #'fitness-function #'fitness-p)
((OR
  (IF
   (OR
    (IF (E-SENSOR GRID-WORLD CURRENT-CELL) (SW-SENSOR GRID-WORLD CURRENT-CELL)
     (OR (NE-SENSOR GRID-WORLD CURRENT-CELL) (N-SENSOR GRID-WORLD CURRENT-CELL)))
    (NE-SENSOR GRID-WORLD CURRENT-CELL))
   (E GRID-WORLD CURRENT-CELL)
   (AND (SE-SENSOR GRID-WORLD CURRENT-CELL)
    (OR (SE-SENSOR GRID-WORLD CURRENT-CELL)
     (AND (W-SENSOR GRID-WORLD CURRENT-CELL) (N-SENSOR GRID-WORLD CURRENT-CELL)))))
  (N GRID-WORLD CURRENT-CELL))
 . 35)
 > (evolve primitives actions conditionals args fargs #'fitness-function #'fitness-p)
((IF
  (OR
   (OR (NE-SENSOR GRID-WORLD CURRENT-CELL)
    (AND (AND (E-SENSOR GRID-WORLD CURRENT-CELL) (SE-SENSOR GRID-WORLD CURRENT-CELL)) (N GRID-WORLD CURRENT-CELL)))
   (S-SENSOR GRID-WORLD CURRENT-CELL))
  (W GRID-WORLD CURRENT-CELL) (S GRID-WORLD CURRENT-CELL))
 . 44)
```
