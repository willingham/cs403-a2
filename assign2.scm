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

(define (run1)
  )

(define (run2)
  )

(define (run3)
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
