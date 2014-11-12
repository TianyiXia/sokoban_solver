;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
;;return true if only if state s is a goal state
;;this works by recursivly checking the left top corner item on the square, if no box were 
;;found after traverse the whole map, return true, else nil
(defun goal-test (s)
  (cond 
        ((null s) T)
        ((isBox(first (first s))) nil)
        (t (goal-test (cleanUpList (cons (rest (first s)) (rest s))))))) ;end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

;;(r,c) assumes 0 based
;;given state s row r colum c, return the item at that location
(defun get-square(s r c)
  (cond ((or (< r 0) (< c 0) (> r (- (length s) 1)) (> c (- (length (first s)) 1))) 1) ;;if out of scope treat as wall
        (t (first (nthcdr c (first (nthcdr r s)) )))))

;;set (r,c) to item v by carefully breaking up lists, inserting v and appending them togather
(defun set-square(s r c v)
  (let* ((x (length s))
         (y (length (first s)))
         (lists-before (butlast s (- x r))) ;;lists before the modifying row
         (change (first (nthcdr r s))) ;;lists to be changed
         (lists-after (rest (nthcdr r s))) ;;lists after the changing part
         (new-list (append (butlast change (- y c)) (list v) (rest (nthcdr c change))))) ;;the changing list is break up to 3 part
    (append lists-before (list new-list) lists-after)))
         

;;can box/player move to (r,c); i.e. is (r, c) an empty/goal square?
;;return true if (r,c) is either empty or goal, nil otherwise
(defun can-move(s r c)
  (let ((v (get-square s r c)))
    (cond ((or (isBlank v) (isStar v)) T)
          (t nil))))
;;return true if (r,c) is verlapped either with box or with keeper, nil otherwise
(defun isOverlapped (s r c)
  (or (= (get-square s r c) 5) (= (get-square s r c) 6)))

;;given state s, and an item v(box/keeper), predict what will be on location (r,c) if item v(box/keeper) move into it.
;;if (r,c) was a goal state, then it will become an overlapping object, else it will become v
(defun move (s r c v)
  (cond ((isStar (get-square s r c))
         (cond ((isBox v) boxstar)
               (t keeperstar)))
        (t v)))
;;see if location (r,c) is a box/boxstar. otherwise return nil
(defun containBox(s r c)
  (or (isBox(get-square s r c)) (isBoxstar (get-square s r c))))

;;4 direction is defined as 1(left) 2(up) 3(right) 4(down)
;;assumming move is valid, move obejct v at (r,c) in particular direction and update two changing squares
;;for each direction, if the object was originally overlapped with goal, that goal need to be restored after moving away, else set it to blank
;;if the object is moving to goal, set new square to 5/6 depending on what object it is,else just set it to v.
(defun apply-move(s r c v dir)
  (cond ((and (= dir 1) (not (isOverlapped s r c))) (set-square (set-square s r (- c 1) (move s r (- c 1) v)) r c blank))
        ((= dir 1) (set-square (set-square s r (- c 1) (move s r (- c 1) v)) r c star))
        
        ((and (= dir 2) (not (isOverlapped s r c))) (set-square (set-square s (- r 1) c (move s (- r 1) c v)) r c blank))
        ((= dir 2) (set-square (set-square s (- r 1) c (move s (- r 1) c v)) r c star))
        
        ((and (= dir 3) (not (isOverlapped s r c))) (set-square (set-square s r (+ c 1) (move s r (+ c 1) v)) r c blank))
        ((= dir 3) (set-square (set-square s r (+ c 1) (move s r (+ c 1) v)) r c star))
        
        ((and (= dir 4) (not (isOverlapped s r c))) (set-square (set-square s (+ r 1) c (move s (+ r 1) c v)) r c blank))
        ((= dir 4) (set-square (set-square s (+ r 1) c (move s (+ r 1) c v)) r c star))))


        
;;move to left, check player's left, if player can move directly, do that (update 2 place)
;;else if that pos is a box/or box on goal, check box's left, if can move, do that (update 3 place)
(defun try-move(s dir)
  (let ((rp (second (getKeeperPosition s 0)))
        (cp (first (getKeeperPosition s 0))))
    (cond ((and (= dir 1) (can-move s rp (- cp 1))) (apply-move s rp cp keeper dir))
          ((and (= dir 1) (containBox s rp (- cp 1)) (can-move s rp (- cp 2))) (apply-move (apply-move s rp (- cp 1) box dir) rp cp keeper dir))
          
          ((and (= dir 2) (can-move s (- rp 1) cp)) (apply-move s rp cp keeper dir))
          ((and (= dir 2) (containBox s (- rp 1) cp) (can-move s (- rp 2) cp)) (apply-move (apply-move s (- rp 1) cp box dir) rp cp keeper dir))

          
          ((and (= dir 3) (can-move s rp (+ cp 1))) (apply-move s rp cp keeper dir))
          ((and (= dir 3) (containBox s rp (+ cp 1)) (can-move s rp (+ cp 2))) (apply-move (apply-move s rp (+ cp 1) box dir) rp cp keeper dir))
          
          ((and (= dir 4) (can-move s (+ rp 1) cp)) (apply-move s rp cp keeper dir))
          ((and (= dir 4) (containBox s (+ rp 1) cp) (can-move s (+ rp 2) cp)) (apply-move (apply-move s (+ rp 1) cp box dir) rp cp keeper dir))
          (t nil))))
  

;;call try move on 4 possible direction.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos)) ;;col
	 (y (cadr pos)) ;;row
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 1) (try-move s 2) (try-move s 3) (try-move s 4)))
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;h1 is admissible, when all boxes are in goal, h1(s)=0,
;;otherwise, it takes at least one move to move a misplaced box to goal and moving one box will not affect other box's position, if h1(s)=n,
;;the total cost must be at least n*1=n. 
(defun h1 (s)
	(cond ((null s) 0)
		(t (+ (count box (first s)) (h1 (rest s)))))
)


; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;


;;given a state and object v which could be box or goal, return the list of all the coordinates of that objects
;;r c is the initial search point, default (0 0), it is used as book keeping information to remember coordinates during recursion
(defun getLocation(s r c v)
  (cond ((null s) nil)
        ;;first check if the initial serach point (left top corner) is matching with v
        ;;there are 3 possible cases:
        ;;1) (r,c) is a star, and v is star; 
        ;;2) (r,c) is a keeper on star, and v is star;
        ;;3) (r,c) is box and v is box
        ;;if any of them is a yes, append current coordinate and recurse on the rest of the map
        ((or (and (isStar v) (or (isStar(first (first s))) (isKeeperStar(first (first s)))) ) 
             (and (isBox v) (isBox(first (first s))) ))
         (cond ((null (rest (first s))) (append (list (list r c)) (getLocation (cleanUpList (append (list (rest (first s))) (rest s))) (+ r 1) 0 v)))
               (t (append (list (list r c)) (getLocation (cleanUpList (append (list (rest (first s))) (rest s))) r (+ c 1) v)))))
        ;;otherwise skip this node and recurse on the rest of the map
        (t
         (cond ((null (rest (first s))) (getLocation (cleanUpList (append (list (rest (first s))) (rest s))) (+ r 1) 0 v))
               (t (getLocation (cleanUpList (append (list (rest (first s))) (rest s))) r (+ c 1) v))))))

;;gievn two point calculate the manhattan distance between the two
(defun distance(p1 p2)
  (let ((r1 (first p1))
        (c1 (second p1))
        (r2 (first p2))
        (c2 (second p2)))
    (+ (abs (- r1 r2)) (abs (- c1 c2)))))

;;l is a list, e is an element of that list
;;assume l is all unique
;;return l with e deleted
(defun exclude (l e)
  (cond ((null l) nil)
        ((equal e (first l)) (rest l))
        (t (cons (first l) (exclude (rest l) e)))))

;;gievn a list  of box points and a list of goal points
;;return a sorted list of goal such that the coresponding manhattan distance is minimun (match each box to its cloest goal)
(defun sortGoal(boxList goalList)
  (cond ((null boxList) goalList)
        ((null goalList) nil)
        (t (let* ((mbox (first boxList))
                 (curr (first goalList)) ;;current candidate of matched goal
                 (alt (first (sortGoal (list mbox) (rest goalList))))) ;;alternative candiate by recursivly calling sortGoal on rest of goalList
             (cond ((null alt) (list curr))
                   ((< (distance curr mbox) (distance alt mbox)) (append (list curr) (sortGoal (rest boxList) (rest goalList))))
                   (t (append (list alt) (sortGoal (rest boxList) (exclude goalList alt)))))))))
                       
;;given already a list of goals and already matched goal list (assume same size), calculate the sum of manhattan distance between each of them
(defun sumDistance (boxList goalList)
  (cond ((null boxList) 0)
        (t (+ (distance (first boxList) (first goalList)) (sumDistance (rest boxList) (rest goalList) )))))

;;given a state s, return ture if position (r c) is a wall, ni otherwise
(defun blocked (s r c)
  (let ((v (get-square s r c)))
    (cond ((isWall v) t)
          (t nil))))

;;gievn a state s and a list of box position, return true if any of them is stuck in a corner (blocked by at least a wall both horizontally and vertically)
(defun isStuck (s boxList)
  (cond ((null boxList) nil)
        (t (let ((r (first (first boxList)))
                 (c (second (first boxList))))
             (cond ((and (or (blocked s (- r 1) c) (blocked s (+ r 1) c)) (or (blocked s r (+ c 1)) (blocked s r (- c 1)))) t)
                   (t (isStuck s (rest boxList))))))))

;;gievn a state s and a list of box position, calculate the number of move to get to the farthest box (notice we want to be near the box, not on top of it)
(defun keeperToBox (s boxList)
  (cond ((null boxList) 0)
        (t 
         (let* ((r (second (getKeeperPosition s 0)))
               (c (first (getKeeperPosition s 0)))
               (curr (- (distance (list r c) (first boxList)) 1))
               (alt (keeperToBox s (rest boxList))))
           (cond ((> curr alt) curr)
                 (t alt))))))
        

;;first check if box is tuck in a corner (at least a wall both horizontally and vertically), if that it the case, return a large number,
;;this will not break adimissible because the actual cost is infinity.
;;then calculate the sum of manhattan distance from each box to its nearest goal.
;;then calculate the sum of moves from player to each box, notice move is distance - 1, when player is adjacent to box, that is 0 move.
;;add them togather
(defun h104129701 (s)
  (let* ((boxs (getLocation s 0 0 box)))
    (cond ((isStuck s boxs) 1000)
          (t 
          (let*  ((goals (getLocation s 0 0 star))
                  (offset (- (length goals) (length boxs)))) ;;if there are more goals than boxs, delete the remaining farthest goals
            (+ (sumDistance boxs (butlast (sortGoal boxs goals) offset)) (keeperToBox s boxs)))))))
            ;;(h1 s))))))
        
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5+9+3+3 7+7+3+3 =20
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
