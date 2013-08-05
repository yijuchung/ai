;; configuration
;; time for bfs 10 is set as default
(setq time-limit-for-bfs 10)
(setq state-level-limit 5000)
(setq total-time-limit 30)
;; larger-group-for-next : leave larger group to next move
;; prev-freq-color : choose previous freq chosen color again
(setq larger-group-for-next 0.3)
(setq prev-freq-color 0.5)

(defun set-val (b x y v)
	(setf (aref b x y) v))

(defun print-board (&optional (b board))
	(setq row (1- num))
	(setq col 0)
	(format t "  ")
	(do()
		((= col num))
		(format t "~2d" col)
		(setq col (1+ col))
	)
	(format t "~%")
	(setq col 0)
	(do()
		((< row 0))
		(when (= 0 col) (format t "~2d " row))
		(setq itm (aref b row col))
		(setq col (1+ col))
		(cond
			( (= col num) (setq row (1- row)) (setq col 0)
			(format t "~A~%" itm))
			(t (format t "~A " itm))))
	(values)
)

(defun load-game-board (&optional (f "data.txt"))
	(setq inf (open f :direction :input) )
	(setq num (parse-integer (read-line inf) ))
	(setq board (make-array (list num num)))
	(setq color-list nil)
	(setq score 0)
	(setq count 0)
	
	(when (> num 5)
		(setq time-limit-for-bfs 13)
	)
	
	(when (> num 10)
		(setq time-limit-for-bfs 20)
	)
	
	(when (> num 20)
		(setq time-limit-for-bfs 25)
	)
	
	(when (> num 24)
		(setq time-limit-for-bfs 15)
	)
	
	;; total num of colors
	(setq color-num 0)
	;; distinct colors in list
	(setq color-list nil)
	;; color current freq count
	(setq color-freq nil)
	;; current remaining color tiles
	(setq color-count nil)
	;; total remaining tiles
	(setq total-tile 0)
		
	(setq row (1- num))
	(setq col 0)

	(do ((itm (read inf) (read inf nil 'eof))) ((< row 0))
		(setf (aref board row col) itm)
		(setq color-list (cons itm color-list))
		(setq col (1+ col))
		(when (= col num) (setq row (1- row)) (setq col 0 )))

	(close inf)
	(update-color-initial)
	(print-board))
	
(defun update-color-initial ()
	(setq color-nd (remove-duplicates color-list))
	(setq color-num (list-length color-nd))
	(setq color-count (make-array (list color-num)))
	
	(setq temp-len (list-length color-list))
	(loop for ic from 0 to (1- color-num) do
		(setq color-list (remove (nth ic color-nd) color-list))
		(setf (aref color-count ic) (- temp-len (list-length color-list)))
		(setq total-tile (+ (aref color-count ic) total-tile))
		(setq temp-len (list-length color-list)))
		
	(setq color-list (make-array color-num :initial-contents color-nd)))
	
(defun num-of-two-max-color (&optional (cc color-count))
	(setq color-count-list nil)
	(loop for ic from 0 to (1- color-num) do
		(push (aref cc ic) color-count-list)
	)
	
	(setq color-count-list (sort color-count-list #'> ))
	
	(return-from num-of-two-max-color (+ (first color-count-list) (second color-count-list)))
)
	
(defun num-of-color (c &optional (cc color-count))
	(loop for ic from 0 to (1- color-num) do
		(when (equal c (aref color-list ic)) (return (aref cc ic)))))
		
(defun minus-num-of-color (c n &optional (cc color-count))
	(loop for ic from 0 to (1- color-num) do
		(when (equal c (aref color-list ic)) (return (setf (aref cc ic) (- (aref cc ic) n))))))

(defun plus-num-of-color (c n &optional (cc color-count) )
	(loop for ic from 0 to (1- color-num) do
		(when (equal c (aref color-list ic)) (return (setf (aref cc ic) (+ (aref cc ic) n))))))
		
(defun print-color-count (&optional (cc color-count) )
	(loop for ic from 0 to (1- color-num) do
		(format t "~A " (aref color-list ic)))
	(format t "~%")
	(loop for ic from 0 to (1- color-num) do
		(format t "~A " (aref cc ic)))
	(format t "~%"))

(defun copy-array (array &key (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)(fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
	(let* ((dimensions (array-dimensions array))
      (new-array (make-array dimensions
		:element-type element-type
		:adjustable adjustable
		:fill-pointer fill-pointer)))
	(dotimes (i (array-total-size array))
	(setf (row-major-aref new-array i)
	(row-major-aref array i)))
		new-array))

(defun mark-recur (x y color &optional (m-color 'N) (b board) )
	(cond 
		((< x 0) nil)
		((= x num) nil)
		((< y 0) nil)
		((= y num) nil)
		((equal (aref b x y) color) (set-val b x y m-color)
			(setq count (1+ count))
			(mark-recur (1- x) y color m-color b)
			(mark-recur (1+ x) y color m-color b)
			(mark-recur x (1- y) color m-color b)
			(mark-recur x (1+ y) color m-color b)
		)
		(t nil)))
		
(defun clean-board-downwards (&optional (b board))
	(setq new-board (make-array (list num num) :initial-element 'N))
	
	(setq i 0) (setq j 0)
	(setq new-i 0) (setq new-j 0)
	(do ()((= j num))
		(cond
			((equal (aref b i j) 'N)
				(setq i (1+ i)))
			(t
				(setf (aref new-board new-i new-j) (aref b i j))
				(setq i (1+ i))(setq new-i (1+ new-i)))
			)
		(when (= i num) (setq i 0) (setq j (1+ j))
			(setq new-i 0)(setq new-j (1+ new-j))	))
	(return-from clean-board-downwards new-board))
	
(defun clean-board-leftwards (&optional (b board))
	(setq new-board (make-array (list num num) :initial-element 'N))
	
	(setq i 0) (setq j 0)
	(setq new-i 0) (setq new-j 0)
	(do ()((= j num))
		(cond
			((equal (aref b 0 j) 'N)
				(setq j (1+ j)))
			(t
				(setf (aref new-board new-i new-j) (aref b i j))
				(setq i (1+ i))(setq new-i (1+ new-i)))
			)
		(when (= i num) (setq i 0) (setq j (1+ j))
			(setq new-i 0)(setq new-j (1+ new-j))	))
			
	(return-from clean-board-leftwards new-board))

(defun check-move (&optional (b board))
;; flag = 0 - game over
	(setq check-board (copy-array b))
	(setq count 0)
	(setq flag 0)
	(setq i 0) (setq j 0)
	
	(do ()((= j num))
		(cond
			((equal (aref check-board i j) 'N)
				(setq j (1+ j)) (setq i 0))
			((equal (aref check-board i j) 'M)
				(setq i (1+ i)))
			(t
				;(format t " i: ~A, j: ~A, color: ~A~%" i j (aref check-board i j))
				(mark-recur i j (aref check-board i j) 'M check-board)
				(cond 
					((> count 2) (setq flag 1)(setq j num))
					(t (setq i (1+ i)) (setq count 0))))
			)
		(when (= i num) (setq i 0) (setq j (1+ j))))
	
	(return-from check-move flag)
	)
	
(defun move (x y)
	(when (< x 0) (format t  "Invalid move~%") (return-from move))
	(when (< y 0) (format t  "Invalid move~%") (return-from move))
	(when (>= x num) (format t  "Invalid move~%") (return-from move))
	(when (>= y num) (format t  "Invalid move~%") (return-from move))
	(setq count 0)
	(setq temp-color (aref board x y))
	(mark-recur x y temp-color)
	(cond
		((< count 3) (format t  "only more than 3 can be counted~%")
			(mark-recur x y 'N temp-color))
		(t 
			(setq new-score (expt (- count 2) 2) )
			(format t "this move gains score: ~A !!~%" new-score )
			(setq score (+ score new-score))
			(format t "current total score: ~A !!~%" score )
			(setq total-tile (- total-tile count))
			(format t "remain tiles: ~A~%" total-tile )
			(minus-num-of-color temp-color count)
			(print-color-count)
			;; clean
			(setq board (clean-board-downwards))
			(setq board (clean-board-leftwards))
			(cond
				((equal (aref board 0 0) 'N)
					(format t  "No more beads in this board, end the game.~%"))
				(t (when (= 0 (check-move)) (format t  "No more moves in this board, end the game.~%")))
				)
			)
		)
	(print-board))
	
;; algo part
(defstruct state
	board
	color-count
	prev-color-count
	total-tile
	score
	move-list
	cand-move-list
)

(defun sim-move (x y s)
; x y , s original state
; state, flag
	;(format t  "sim move ~A ~A.~%" x y)
	(setq count 0)
	(setq temp-color (aref (state-board s) x y))
	(mark-recur x y temp-color 'N (state-board s))
	(setq new-score (expt (- count 2) 2) )
	(setf (state-score s) (+ (state-score s) new-score))
	(setf (state-total-tile s) (- (state-total-tile s) count))
	(minus-num-of-color temp-color count (state-color-count s))
	(setf (state-board s) (clean-board-downwards (state-board s)))
	(setf (state-board s) (clean-board-leftwards (state-board s)))
	(return-from sim-move (check-move (state-board s)))
)

(defun priority-sort (a b)
	(cond
		((null a) (not (null b)))
		((null b) nil)
		((= (fifth a) (fifth b))
			(> (cadr a) (cadr b))
		)
		(t (> (fifth a) (fifth b)))
	)
)
(defun analyze-board (s &optional (f 0))
;; s state, f 0-priority
;; move - color, count, x, y

	(setq a-board (copy-array (state-board s)))
	(setq cur-cand-move nil)
	
	(loop for i from 0 to (1- num) do
		(loop for j from 0 to (1- num) do
			(setq cur-color (aref a-board i j))
			(when (not (equal cur-color 'N))
				(setq count 0)
				(mark-recur i j cur-color 'N a-board)
				(when (> count 2)
					(setq key (- (state-total-tile s) count))
					(setq key (* key larger-group-for-next))
					(setq key (+ key (* (num-of-color cur-color (state-color-count s)) prev-freq-color)))
					(push (list cur-color count i j key) cur-cand-move)
				)
			)
		)
	)
	(setf (state-cand-move-list s) (sort (copy-seq cur-cand-move)  #'priority-sort))
)

(defun copy-struct (os)
	(return-from copy-struct (make-state :board (copy-array (state-board os)) :color-count (copy-array (state-color-count os)) :total-tile (state-total-tile os) :score (state-score os) :move-list (copy-list (state-move-list os)) :prev-color-count (copy-array (state-prev-color-count os))))
)

(defun compare-state (s1 s2)
;; T - s1 < s2, nil - s1 >= s2
	(cond
		((null s1) (not (null s2)))
		((null s2) nil)
		((= (state-total-tile s1) (state-total-tile s2)) (> (state-score s1) (state-score s2)))
		(t (< (state-total-tile s1) (state-total-tile s2)))
	)
)

(defun sort-end-state (&optional (sl state-level))
	(return-from sort-end-state (sort sl #'compare-state))
)

(defun filter-end-state (s)
;; 1 - keep, 0 - drop
	(cond 
		((< (state-total-tile s) cur-best-tile)
			(setq cur-best-tile (state-total-tile s))
			(setq cur-best-score (state-score s))
			(return-from filter-end-state 1)
		)
		((= (state-total-tile s) cur-best-tile)
			(when (> (state-score s) cur-best-score)
				(setq cur-best-score (state-score s))
			)
			(return-from filter-end-state 1)
		)
		(t
			(return-from filter-end-state 0)
		)
	)
)

(defun sort-state-level (s1 s2)
	(setq state-order-s1 (float (/ (num-of-two-max-color (state-color-count s1)) (state-total-tile s1))))
	(setq state-order-s2 (float (/ (num-of-two-max-color (state-color-count s2)) (state-total-tile s2))))
	
	(cond
		((null s1) (not (null s2)))
		((null s2) nil)
		((= state-order-s1 state-order-s2) (> (num-of-two-max-color (state-color-count s1)) (num-of-two-max-color (state-color-count s2))))
		(t (> state-order-s1 state-order-s2))
	)
)

(defun auto-play ()
	(setq state-level nil)
	(setq state-next-level nil)
	(setq end-state nil)
	(setq state-stack nil)
	(setq cur-best-tile total-tile)
	(setq cur-best-score 0)
	
	(setq time-elapse 0)
	(BFS)
	(DFS)
	
	(when (null end-state)
		(format t "sorry couldn't find any solution this time.~%")
		(return-from auto-play)
	)
	
	(setq best-state (car (sort-end-state end-state)))
	;(format t "~A~%" (state-move-list best-state))
	(setq final-move-list (reverse (state-move-list best-state)))
	
	(loop for x in final-move-list do
		(move (first x) (second x))
	)
	
	(format t "finding best moves in ~A secs. ~%" time-elapse)
	(format t "left beads : ~A~%" (state-total-tile best-state))
	(format t "score : ~A~%" (state-score best-state))
	
	(values)
)

(defun BFS ()
	;(setq thres 10)
	;(setq depth 1)
	
	(setq root-state (make-state :board (copy-array board) :prev-color-count (make-array color-num :initial-element 0) :color-count (copy-array color-count) :total-tile total-tile :score 0))
	
	;(setf (state-cand-move-list root-state) (analyze-board root-state))
	(analyze-board root-state)
	(push root-state state-level)
	
	(setq filter-out-num 0)
	
	(setq time-last (get-internal-real-time))
	(loop 
		(when (null state-level)
			(if (null state-next-level)
				(progn
					(setq time-current (get-internal-real-time))
					(setq time-elapse (+ time-elapse (float (/ (- time-current time-last) internal-time-units-per-second))))
					;(princ 1)
					(return-from BFS)
				)
				(progn
					(setq state-level state-next-level)
					(setq state-next-level nil)
					;(setq depth (1+ depth))
					;(princ 2)
					
					;(setq time-current (get-internal-real-time))
					
					(when (> (list-length state-level) state-level-limit)
						(setq time-current (get-internal-real-time))
						(setq time-elapse (+ time-elapse (float (/ (- time-current time-last) internal-time-units-per-second))))
						(format t "over expanding node limit go to DFS ~%")
						;(setq state-level (sort state-level #'sort-state-level))
						(return-from BFS)
					)
					
					;(when (= depth thres)
					;	;(setq state-level (sort-state-level state-level))
					;	(format t "switch to dfs !! ~%")
					;	(return-from BFS)
					;)
				)
			)
		)
		(setq cur-state (pop state-level))
		;(setf (state-cand-move-list cur-state) (analyze-board (state-board cur-state)))
		(analyze-board cur-state)
		(setq child-state (copy-struct cur-state))
		
		(loop while (not (null (state-cand-move-list cur-state))) do
			(setq m (pop (state-cand-move-list cur-state)))
			(setq flag-over (sim-move (third m) (fourth m) child-state))
			
			(if (= flag-over 0)
				(progn
					;; filter out losing end state
					;(when (= (filter-end-state child-state) 1)
					;	(push (list (third m) (fourth m)) (state-move-list child-state))
					;	(push (copy-struct child-state) end-state)
					;)
					(push (list (third m) (fourth m)) (state-move-list child-state))
					(push (copy-struct child-state) end-state)
				)
				(progn
					(plus-num-of-color (first m) 1 (state-prev-color-count child-state))
					(push (list (third m) (fourth m)) (state-move-list child-state))
					(push (copy-struct child-state) state-next-level)
				)
			)
			
			(setq child-state (copy-struct cur-state))
			
			(setq time-current (get-internal-real-time))
			(when (> (+ time-elapse (float (/ (- time-current time-last) internal-time-units-per-second))) time-limit-for-bfs)
				(setq time-elapse (+ time-elapse (float (/ (- time-current time-last) internal-time-units-per-second))))
				(format t "over time limit for BFS go to DFS ~%")
				;(setq state-level (sort state-level #'sort-state-level))
				(return-from BFS)
			)
		)
	)
)

(defun DFS ()
	(when (null state-level)
		(return-from DFS)
	)
	
	(setq state-stack nil)
	
	(setq time-last (get-internal-real-time))
	(loop
		(when (null state-stack)
			(if (null state-level)
				(progn
					(setq time-current (get-internal-real-time))
					(setq time-elapse (+ time-elapse (float (/ (- time-current time-last) internal-time-units-per-second))))
					(return-from DFS)
				)
				(progn
					(setq time-current (get-internal-real-time))
					
					(setq time-diff (+ time-elapse (float (/ (- time-current time-last) internal-time-units-per-second))))
					(when (> time-diff total-time-limit)
						(setq time-elapse time-diff)
						(format t "running time over ~A seconds, break it out !! ~%" total-time-limit)
						(return-from DFS)
					)
					
					(push (pop state-level) state-stack)
					;(setq time-last (get-internal-real-time))
				)
			)
		)
		
		(setq cur-state (pop state-stack))
		(analyze-board cur-state)
		(setq child-state (copy-struct cur-state))
		
		(loop while (not (null (state-cand-move-list cur-state))) do
			(setq m (pop (state-cand-move-list cur-state)))
			(setq flag-over (sim-move (third m) (fourth m) child-state))
			
			(if (= flag-over 0)
				(progn
					;; filter out losing end state
					;(when (= (filter-end-state child-state) 1)
					;	(push (list (third m) (fourth m)) (state-move-list child-state))
					;	(push (copy-struct child-state) end-state)
					;)
					(push (list (third m) (fourth m)) (state-move-list child-state))
					(push (copy-struct child-state) end-state)
				)
				(progn
					(plus-num-of-color (first m) 1 (state-prev-color-count child-state))
					(push (list (third m) (fourth m)) (state-move-list child-state))
					(push (copy-struct child-state) state-stack)
				)
			)
			
			(setq child-state (copy-struct cur-state))
			
			(setq time-current (get-internal-real-time))
			(setq time-diff (+ time-elapse (float (/ (- time-current time-last) internal-time-units-per-second))))
			;(format t "~A~%" time-diff)
			(when (> time-diff total-time-limit)
				(setq time-elapse time-diff)
				(format t "running time over ~A seconds, break it out !! ~%" total-time-limit)
				(return-from DFS)
			)
		)
	)
)

;; main program
(load-game-board)	