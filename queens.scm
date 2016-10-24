(define (make-position row col)
   (cons row col))

(define (position-row position)
   (car position))

(define (position-col position)
   (cdr position))

(define empty-board nil)

(define (filter pred lst)
  (cond ((null? lst) nil)
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (accumulate op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
   (accumulate append nil (map proc seq)))

;;----------------------------------------------------

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (adjoin-position row col positions)
   (append (list (make-position row col)) positions))

(define (safe? col positions)
    (let (
          (kth-queen 
                (car  positions ))
          (other-queens 
                (filter (lambda (q) (not (= col (position-row q)))) positions)))
   (define (attacks? q1 q2)
     (or (= (position-col q1) (position-col q2))
         (= (abs (- (position-row q1) (position-row q2)))
            (abs (- (position-col q1) (position-col q2))))))

   (define (iter q board)
     (or (null? board)
         (and (not (attacks? q (car board)))
              (iter q (cdr board)))))
   (iter kth-queen other-queens)))

(define (queens board-size)
   (define (queen-cols k) 
     	(if (= k -1)
         	(list empty-board)
         	(filter
          		(lambda (positions) (safe? k positions))
          		(flatmap
           		    (lambda (rest-of-queens)
             	        (map (lambda (new-row)
                            (adjoin-position k new-row rest-of-queens))
                            (enumerate-interval 0 (- board-size 1))
                            )
                        )
                    (queen-cols (- k 1))
                    )
                )
		    )
	    )
    (queen-cols (- board-size 1))
	)

(inspect (queens 1))
(inspect (queens 2))
(inspect (queens 3))
(inspect (queens 4))
