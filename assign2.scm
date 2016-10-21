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

(define (run1)
    (iterate i (list 1 2 3 4)
        (inspect i)
        (inspect (* i i))
        )
    )

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

(define (run2)
	(define a 1)
	(define b 2)
	(define c 3)
    (define . 'MISSING)
    (define (f x y z) (+ x y z))
    (exprTest (f a b c) 6)
    (exprTest ((peval f . . .) a b c) 6)
    (exprTest ((peval f . b .) a c) 6)
    (exprTest ((peval f . . c) a b) 6)
    (exprTest ((peval f a . c) b) 6)
    (exprTest ((peval f a b c)) 6)
    )
		
;;; trois

(define (Stack) 
    (list (list ) 0)
    )

(define (push s x)
    (if (null? s)
        (list (list x) (+ (cadr x) 1))
        (list (cons x (car s)) (+ (cadr s) 1))
		)
    )

(define (pop s)
    (if (equal? (ssize s) 0)
        s
        (list (cdr (car s)) (- (cadr s) 1))
        )
    )

(define (speek s)
    (if (null? s)
        s
        (caar s)
        )
    )

(define (ssize s)
        (cadr s)
    )

(define (Queue)
    (list (Stack) (Stack))
    )

(define (enqueue q x)
    (list (car q) (push (cadr q) x))
    )

(define (dequeue q)
    (if (null? (car (cadr q)))
        (list (pop (car q)) (cadr q))
        (dequeue (list (push (car q) (speek (cadr q))) (pop (cadr q))))
        )
    )

(define (qpeek q)
    (if (equal? (ssize (cadr q)) 0)
        (speek (car q))
        (qpeek (list (push (car q) (speek (cadr q))) (pop (cadr q))))
        )
    )

(define (qsize q)
    (+ (ssize (car q)) (ssize (cadr q)))
    )

;;; quatre

(define (no-locals orig)
    (define (noLocalsIt func args cur)
        (if (and (list? (car cur)) (== (car (car cur)) 'define))
            (if (== (length func) 0)
                (if (== (length args) 0)
                    (noLocalsIt (list (cadr (car cur))) (list (caddr (car cur))) (cdr cur))
                    (noLocalsIt (list (cadr (car cur))) (append args (list (caddr (car cur)))) (cdr cur))
                    )
                )
            (append (list (list 'lambda func (car cur))) args)
            )
    )
    (list (car orig) (cadr orig) (noLocalsIt '() '() (cddr orig)))
)

(define (run4)
   	(exprTest (no-locals (quote (define (nsq a) (define x (+ a 1)) (* x x)))) '(define (nsq a) ((lambda (x) (* x x)) (+ a 1))))
	) 

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
       
(define (run5)
    (define three (lambda (f) (lambda (x) (f (f (f x))))))
    (inspect (define two (pred three)))
    (exprTest ((two (lambda (z) (+ 1 z))) 0) 2)
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

(println "assignment 2 loaded!")
