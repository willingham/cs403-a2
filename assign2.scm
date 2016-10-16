(define (author)
    (println "AUTHOR: Thomas Willingham twillingham@crimson.ua.edu")
    )

(define (iterator # $x items $)
    (define (join holder items $)
        (if (valid? car(x)))
            
        )
    )

(define (for-loop arglist procedure)
  (cond
    ((null? arglist) nil)
    (else (begin
      (procedure (car arglist))
      (for-loop (cdr arglist) procedure)
      ))
    )
  )

(define (for-loop arg procedureList finalList)
  (cond
    ((null? procedureList) (println finalList))
    (else (begin
      (for-loop arg (cdr procedureList) (cons arg (car procedureList)))
      ))
    )
  )



(for-loop 1  (list (lambda (x) (inspect x))) (list ))

(define (Stack) 
    (list )
    )

(define (push s x)
    (if (null? s)
        (list x)
        (append (list x) s)
        )
    )

(define (pop s)
    (if (null? s)
        '()
        (cdr s)
        )
    )

(define (speek s)
    (if (null? s)
        '()
        (car s)
        )
    )

(define (ssize s)
    (if (null? s)
        0
        (length s)
        )
    )

(define (Queue)
    (list (Stack) (Stack))
    )

(define (enqueue q x)
    (list (car q) (push x (cadr q)))
    )

(define (dequeue q)
    (if (null? (cadr q))
        (list (pop (car q)) (cadr q))
        (dequeue (list (cons (peek (cadr q)) (car q)) (pop (cadr q))))
        
        ;(list (car q) (cdr (cadr q)))
        ;(dequeue (list (cdr (car q)) (cons (car (car q)) (cdr q))
        )
    )

(define (qpeek)
    (if (null? (cadr q))
        (speek (car q))
        (speek (cadr q))
        )
    )

(define (qsize q)
    (+ (ssize (car q)) (ssize (cadr q)))
    )


  
(define (run1)
  )

(define (run2)
  )

(define (run3)
    (define (loop stack queue)
        (define x (readInt))
        (if (eof?)
            (list stack queue)
            (loop (push stack x) (enqueue queue x))
            )
        )
    (define (popper s)
        (cond
            ((!= (ssize s) 0)
                (inspect (speek s))
                (popper (pop s))
                )
            )
        )
    (define (dequeuer q)
        (cond
            ((!= (qsize q) 0)
                (inspect (speek q))
                (dequeuer (dequeue q))
                )
            )
        )

    (define oldstream (setPort (open "data.ints" 'read)))
    (define data (loop (Stack) (Queue)))
    (popper (car data))
    (dequeuer (cadr data))
    (setPort oldStream)
  )

(define (run4)
  )

(define (run5)
  )

(define (run6)
  )

(define (run7)
  )

(define (run8)
  )

(define (run9)
  )

(define (run10)
  )

