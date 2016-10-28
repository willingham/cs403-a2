(define (newPosition r c)
   (cons r c))

(define (getRow pos)
   (car pos))

(define (getCol pos)
   (cdr pos))

(define newBoard nil)

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

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (addPosition row col positions)
   (append (list (newPosition row col)) positions))

(define (safe? col positions)
    (let (
          (cur 
                (car  positions ))
          (others 
                (filter (lambda (q) (not (= col (getRow q)))) positions)))
    (define (attacks? q1 q2)
        (or (= (getCol q1) (getCol q2))
            (= (abs (- (getRow q1) (getRow q2)))
                (abs (- (getCol q1) (getCol q2))))))

   (define (iter q board)
        (or (null? board)
            (and (not (attacks? q (car board)))
                (iter q (cdr board))
                )
            )
        )
    (iter cur others))
    )

(define (queens size)
   (define (getPostitions n) 
     	(if (= n -1)
         	(list newBoard)
         	(filter
          		(lambda (positions) (safe? n positions))
          		(flatmap
           		    (lambda (otherQueens)
             	        (map (lambda (newRow)
                            (addPosition n newRow otherQueens))
                            (enumerate-interval 0 (- size 1))
                            )
                        )
                    (getPostitions (- n 1))
                    )
                )
		    )
	    )
    (getPostitions (- size 1))
	)

(inspect (queens 1))
(inspect (queens 2))
(inspect (queens 3))
(inspect (queens 4))
(inspect (length (queens 8)))
