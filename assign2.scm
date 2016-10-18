(define (author)
    (println "AUTHOR: Thomas Willingham twillingham@crimson.ua.edu")
    )

(define (exprTest # $expr target)
    (define result (catch (eval $expr #)))
        (if (error? result)
            (println $expr " is EXCEPTION: " (result 'value)
                " (it should be " target ")")
            (println $expr " is " result
                " (it should be " target ")")
            )
        )


;;; un

(define (iterate # $x items $)
	(define y  (eval (list 'lambda  (list $x) (cons 'begin $)) #))
	(map y items)
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

;;; deaux

(define (peval f @)
    (define at @)
	(define (listIt x y z)
		(cond
			((and (null? x) (null? y))
				z
				)
			((null? x)
				(listIt x (cdr y) (cons (car y) z))
				)
			((equal? (car x) 'MISSING)
				(listIt (cdr x) (cdr y) (cons (car y) z))
				)
            (else
                (listIt (cdr x) y (cons (car x) z))
                )
			)
		)   
    (lambda (@)  (apply f  (reverse (listIt at @ ()))))
   
	)
		
;;; trois

(define (Stack) 
    (list )
    )

(define (push s x)
    (if (null? s)
        (list x)
        (cons x s)
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

;;; quatre

(define (no-locals x))

;;; cinq

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

(define pred
    (lambda (n)
        (lambda (f)
            (lambda (x) 
                (cdr ((n (prefn f)) (cons #t x)))
                )
            )
        )
    )
        
;;; six

(define (treeNode value left right)
    (list value left right)
    )

(define (treeflatten t))


(define (treedepth t))

;;; sept

(define (queens n))

;;; huit

(define (cxr x)
    (lambda (y)
        )
    )

;;; neuf

(define old+ +)
    (define old- -)
    (define old* *)
    (define old/ /)
    (define (install-generic)
        (clearTable)
        (set! + (lambda (a b) (apply-generic '+ a b)))
        (set! - (lambda (a b) (apply-generic '- a b)))
        (set! * (lambda (a b) (apply-generic '* a b)))
        (set! / (lambda (a b) (apply-generic '/ a b)))
        (putTable '+ '(STRING STRING) addStrings)
        (putTable '* '(STRING INTEGER) mulStringInteger)
        ;other functions installed here
        'generic-system-installed
        )

(define (install-generic)
	)

(define (uninstall-generic)
        (set! + old+)
        (set! - old-)
        (set! * old*)
        (set! / old/)
        'generic-system-uninstalled
        )

(define (apply-generic))

;;; dix

(define (install-coercion)
	)

(define (coerce n x))

;;; run functions
  
(define (run1)
    )

(define (run2)
	(define (f x y z) (+ x y z))
  	(exprTest ((((peval f) 1) 2) 3) 6)
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
