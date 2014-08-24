; Copyright 2014 Sak Lee (c) All rights reserved
; Simple sudoku solver using constraint propagation and backtracking search
; Receives the sudoku configuration (empty=0) and prints solution
;
; TODO: implement backtrack search

(use srfi-1 srfi-69 srfi-42 data-structures)

; function composition macro. ((compose f g) x) is equal to (f (g (x)))
(define-syntax compose
  (syntax-rules ()
    ((compose)
       (lambda args
         (apply values args)))
    ((compose f . fs)
       (lambda args
         (f (apply (compose . fs) args))))))


; helper function for hash-table-equal?
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
    (map
      (cut hash-table-set! board <> (let ((num (read)))
                                      (if (= num 0)
                                          '(1 2 3 4 5 6 7 8 9)
                                          `(,num))))
      board-coord)
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

; for choosing variable
(define (minimum-remaining-value board)
  (let* ((keys (hash-table-keys board))
         (nums (map length (map (cut hash-table-ref board <>) keys)))
         (keynum-pair (sort (zip nums keys) (lambda (x y) (<= (car x) (car y)))))
         (keynum-pair-over-1 (filter (compose (cut < 1 <>) car) keynum-pair)))
    (cadar keynum-pair-over-1)))

; for choosing value
(define (least-constraining-value board coord)
  (letrec ((choices ; all possible choices on the coord
             (hash-table-ref board coord))
           (adjacents-content ; creates a fresh copy of list containing all adjacents' choices
             (lambda () (map (cut hash-table-ref board <>) (adjacent-coords coord))))
           (delete-from-adjacents ; returns a fresh copy of list of all adjacents minus num
             (lambda (num) (map (cut remove (cut = num <>) <>) (adjacents-content))))
           (extract-minimum-count ; returns the most constrained count
             (lambda (num) (min (map length (delete-from-adjacents num))))))
    (let* ((constraint-measure ; most constrained count per each choice
             (map extract-minimum-count choices))
           (constraint-choices-pair
             (sort (zip constraint-measure choices) (lambda (x y) (>= (car x) (car y))))))
      (cadar constraint-choices-pair))))

(define (check-if-done board)
  (fold (lambda (x y) (and x y)) #t (map (compose (cut = 1 <>) length (cut hash-table-ref board <>)) board-coord)))

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
             (backtrack-search new-board)
             (let* ((bd new-board)
                    (new-bd (improve-arc-consistency (hash-table-copy bd))))
               (cac bd new-bd))))));
    (cac (make-hash-table) board)))

; TODO: implement backtrack search
(define (backtrack-search board)
  (if (check-if-done board)
      board
      (error "failed to find a solution")))

(define (print-board board)
  (let* ((solution (map (cut hash-table-ref board <>) board-coord))
         (coord-sol (zip board-coord solution)))
    (for-each (lambda (x) (display (cadr x)) (if (= 9 (cadar x)) (newline))) coord-sol)))

(define (main)
  (let ((board (load-board)))
    ;(print-board board)
    (print-board (check-arc-consistency board))))

(main)

;(for-each (lambda (x) (display (adjacent-coords x))) board-coord)
