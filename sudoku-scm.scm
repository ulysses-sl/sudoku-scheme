; Copyright (c) 2014 Sak Lee All Rights Reserved
; Direct any question, concern, or reporting to mail@saklee.net
; Simple sudoku solver using constraint propagation and backtracking search
; Receives the sudoku configuration (empty=0) and prints solution
;

(use srfi-1 srfi-42 srfi-69 srfi-78 data-structures utils)

; tells which coordinate comes first between '(r1 c1) and '(r2 c2)
(define (coord-less? coord1 coord2)
  (cond ((and (null? coord1) (null? coord2))
          #t)
        ((< (car coord1) (car coord2))
          #t)
        ((> (car coord1) (car coord2))
          #f)
        (else
          (coord-less? (cdr coord1) (cdr coord2)))))

; to check if check-arc-consistency can no longer improve the board
(define (hash-table-equal? ht1 ht2)
  (let*
    ((ht1-keys (sort (hash-table-keys ht1) coord-less?))
     (ht2-keys (sort (hash-table-keys ht2) coord-less?))
     (ht1-vals (map (cut hash-table-ref ht1 <>) ht1-keys))
     (ht2-vals (map (cut hash-table-ref ht2 <>) ht2-keys)))
    (equal? ht1-vals ht2-vals)))


; load the board from input
; return the board in form of hash ((r c) : '(possible choices))
(define (load-board)
  (let ((board (make-hash-table size: 81)))
    (map (cut hash-table-set! board <> (let ((num (read))) (if (= num 0) '(1 2 3 4 5 6 7 8 9) `(,num)))) board-coord)
    board))


; list containing all board coordinates
(define board-coord
  (list-ec (: i 1 10) (: j 1 10) (list i j)))


; coords that are influenced by the current coord
(define (adjacent-coords coord)
  (let* ((r (car coord))
         (c (cadr coord))
         (r0 (+ 1 (* 3 (quotient (- r 1) 3))))
         (c0 (+ 1 (* 3 (quotient (- c 1) 3)))))
    (remove (cute equal? coord <>) (lset-union
                                     equal?
                                     (list-ec (: i 1 10) (list r i))
                                     (list-ec (: i 1 10) (list i c))
                                     (list-ec (: i r0 (+ r0 3)) (: j c0 (+ c0 3)) (list i j))))))


; returns the list of coordinates sorted by the least number of remaining values
(define (minimum-remaining-value board)
  (let* ((keys (hash-table-keys board))
         (nums (map length (map (cut hash-table-ref board <>) keys)))
         (keynum-pair (sort (zip nums keys) (lambda (x y) (<= (car x) (car y)))))
         (keynum-pair-over-1 (filter (compose (cut < 1 <>) car) keynum-pair)))
    (map cadr keynum-pair-over-1)))


; returns the list of values sorted by the most freedom in the adjacent after choice
(define (least-constraining-value board coord)
  (letrec ((choices ; all possible choices on the coord
             (hash-table-ref board coord))
           (adjacents-content ; creates a fresh copy of list containing all adjacents' choices
             (lambda () (map (cut hash-table-ref board <>) (adjacent-coords coord))))
           (delete-from-adjacents ; returns a fresh copy of list of all adjacents minus num
             (lambda (num) (map (cut remove (cut = num <>) <>) (adjacents-content))))
           
           (extract-new-count ; returns the count of choices after deleting
             (lambda (num) (apply + (map length (delete-from-adjacents num))))))
    (let* ((old-count ; the count of choices before deleting
             (apply + (map length (adjacents-content))))
           (constraint-measure ; most constrained count per each choice
             (map (lambda (num) (- old-count (extract-new-count num))) choices))
           (constraint-choices-pair
             (sort (zip constraint-measure choices) (lambda (x y) (>= (car x) (car y))))))
      (map cadr constraint-choices-pair))))


(define (fold-and args)
  (if (null? args)
      #t
      (and (car args) (fold-and (cdr args)))))


(define (fold-or args)
  (if (null? args)
      #f
      (or (car args) (fold-or (cdr args)))))


; checks if every slot has only one number
(define (is-it-done? board)
  (fold-and (map (compose (cut = 1 <>) length (cut hash-table-ref board <>)) board-coord)))


; has it failed?
(define (has-it-failed? board)
  (call/cc
    (lambda (c)
      (for-each (compose (lambda (x) (if (= x 0) (c #t))) length (cut hash-table-ref board <>)) board-coord)
      #|(for-each (lambda (x) (if (collision? board x) (c #t))) board-coord)|#
      #f)))


; delete a number from coordinate's list
(define (hash-num-remove! bd coord num)
  (let ((val (hash-table-ref bd coord)))
    (hash-table-set! bd coord (remove (cut = num <>) val))))


; helper function to check-arc-consistency
(define (improve-arc-consistency board)
  (letrec
    ((binary-improve ; delete number from adjacent slots for arc consistency.
       (lambda (bd coord)
         (if (= 1 (length (hash-table-ref bd coord)))
             (for-each
               (cut hash-num-remove! bd <> (car (hash-table-ref bd coord)))
               (adjacent-coords coord))))))
    (for-each (cut binary-improve board <>) board-coord)
    board))


; check arc consistency, and do backtrack-search when done
(define (check-arc-consistency board)
  (letrec
    ((cac
       (lambda (board new-board)
         (if (hash-table-equal? board new-board)
             new-board
             (let* ((bd new-board)
                    (new-bd (improve-arc-consistency (hash-table-copy bd))))
               (cac bd new-bd))))));
    (cac (make-hash-table) board)))


; non-destructive number set on coord & delete from a coord's adjacent list
(define (board-number-set board coord num)
  (let ((new-board (hash-table-copy board))
        (adj (adjacent-coords coord)))
    (for-each (cut hash-num-remove! new-board <> num) adj)
    (hash-table-set! new-board coord `(,num))
    new-board))


; backtracking search using MRV and LCV heuristics
(define (backtracking-search board)
  (letrec
    ((bts
       (lambda (bd c)
         ;(display "current board:") (newline)
         (print-board bd) (newline)
         (cond ((is-it-done? bd)
                 (c bd))
               ((has-it-failed? bd)
                 (begin (set! failed-num (+ failed-num 1)) #f))
               (else
                 (let ((vars (minimum-remaining-value bd)))
                   (for-each (cut check-each-val bd <> c) vars))))))
     (check-each-val
       (lambda (bd coord c)
         (for-each (compose (cut bts <> c) check-arc-consistency (cut board-number-set bd coord <>))
                   (least-constraining-value bd coord)))))
    (let ((solution (call/cc (lambda (c) (bts board c)))))
      (if solution
          solution
          (error "Failed to find a solution")))))

(define (print-board board)
  (system* "tput reset")
  (display "failed: ") (display failed-num) (display " times") (newline)
  (let* ((solution (map (cut hash-table-ref board <>) board-coord))
         (coord-sol (zip board-coord solution)))
    (for-each (lambda (x)
                (display (length (cadr x)))
                (if (= 9 (cadar x)) (newline) (display " ")))
              coord-sol)))

(define (main)
  (let ((board (load-board)))
    (print-board (backtracking-search (check-arc-consistency board)))))

(main)
