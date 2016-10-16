(define (author)
    (println "AUTHOR: Thomas Willingham twillingham@crimson.ua.edu")
    )

(define (iterate # $x items $)
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


;;; two

(define (peval f $))

;;; three

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
    (list (car q) (push (cadr q) x))
    )

(define (dequeue q)
    (if (null? (cadr q))
        (list (pop (car q)) (cadr q))
        (dequeue (list (cons (speek (cadr q)) (car q)) (pop (cadr q))))
        )
    )

(define (qpeek q)
    (if (null? (cadr q))
        (speek (car q))
        (qpeek (list (cons (speek (cadr q)) (car q)) (pop (cadr q))))
        )
    )

(define (qsize q)
    (+ (ssize (car q)) (ssize (cadr q)))
    )

;;; four

(define (no-locals x))

;;; five

(define prefn
    (lambda (f)
        (lambda (p)
            (cons #f
                  (if (car p)
                        (cdr p)
                        (f (cdr p))
                        )
                  )
            )
        )
    )

(define pre
    (lambda (n)
        (lambda (f x) 
            (cdr (n (prefn f) (cons #t x)))
            )
        )
    )

;;; six

(define (treedepth))

;;; sept

(define (queens))

;;; huit

(define (cxr))

;;; neuf

(define (apply-generic))

;;; dix

(define (coerce))

;;; run functions
  
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
                (inspect (qpeek q))
                (dequeuer (dequeue q))
                )
            )
        )

    (define oldstream (setPort (open "data.ints" 'read)))
    (define data (loop (Stack) (Queue)))
    (popper (car data))
    (dequeuer (cadr data))
    (setPort oldstream)
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

(println "assignment 2 loaded")
