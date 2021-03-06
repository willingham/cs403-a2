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
    (define (moveAll r)
        (if (equal? (ssize (cadr r)) 0)
            r
            (moveAll (list (push (car r) (speek (cadr r))) (pop (cadr r))))
            )
        )
    (define (getDequeue r)
        (define r (moveAll r))
        (list (pop (car r)) (cadr r))
        )
    (if (equal? (ssize (car q)) 0)
        (getDequeue q)
        (list (pop (car q)) (cadr q))
        )
    )

(define (qpeek q)
    (define (moveAll r)
        (if (equal? (ssize (cadr r)) 0)
            r
            (moveAll (list (push (car r) (speek (cadr r))) (pop (cadr r))))
            )
        )
    (if (equal? (ssize (car q)) 0)
        (speek (car (moveAll q)))
        (speek (car q))
        )
    )

(define (qsize q)
    (+ (ssize (car q)) (ssize (cadr q)))
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
    (println "\n\n new queue\n")
    
    (define q (dequeue (enqueue (dequeue (enqueue (enqueue (Queue) 1) 2)) 3)))
    (exprTest (qpeek q) 3)
    (define q (enqueue q 4))
    (exprTest q "(( 0) ((4 3) 2))")
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

(define pred
    (lambda (n)
        (lambda (f)
            (lambda (x)
                (((n (lambda (g)
                        (lambda (h)
                            (h (g f))
                            )
                        ))
                   (lambda (u) x))
                   (lambda (u) u)
                   )
                )
            )
        )
    )
       
(define (run5)
    (define four (lambda (f) (lambda (x) (f (f (f (f x)))))))
    (inspect (define three (pred four)))
    (exprTest ((three (lambda (z) (+ 1 z))) 0) 3)
    (inspect (define two (pred three)))
    (exprTest ((two (lambda (z) (+ 1 z))) 0) 2)
    )

;;; six

(define (treeNode value left right)
    (list value left right)
    )

(define (treeflatten t)
    (define (treeIter t depth)
        (define left (cadr t))
        (define right (caddr t))
        (define val (car t))
        (cond
            ((and (null? left) (null? right))
                (list (list depth val)))
            ((null? left)
                (treeIter right (+ depth 1)))
            ((null? right)
                (treeIter left (+ depth 1)))
            (else
                (define lh (treeIter left (+ depth 1)))
                (define rh (treeIter right (+ depth 1)))
                (append rh lh)
                )
            )
        )
    (treeIter t 0)
    )

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (treedepth t)
   	(define l (treeflatten t))
	(define len (length l))
	(define lam (lambda (x) (car x)))
	(define m (map lam l))
	(define a (accumulate + 0 m))
	(/ a len)
    )

(define (run6)
  (define n7 (treeNode 7 nil nil))
  (define n6 (treeNode 6 nil nil))
  (define n5 (treeNode 5 n7 n7))
  (define n4 (treeNode 4 n7 n7))
  (define n3 (treeNode 3 n6 n5))
  (define n2 (treeNode 2 n4 n3))
  (define n1 (treeNode 1 n2 n3))
  (inspect (define l (treeflatten n1)))
  (inspect (define len (length l)))
  (inspect (define lam (lambda (x) (car x))))
  (inspect (define m (map lam l)))
  (inspect (define a (accumulate + 0 m)))
  (inspect (/ a len))
  (exprTest (treedepth n1) 3)
  )


;;; sept

(define (queens size)
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

(define (run7)
	(exprTest (queens 1) "(((0 . 0)))")
	(exprTest (queens 2) "nil")
	(exprTest (queens 3) "nil")
	(exprTest (queens 4) "(((3 . 2) (2 . 0) (1 . 3) (0 . 1)) ((3 . 1) (2 . 3) (1 . 0) (0 . 2)))")
	)

;;; huit

(define (cxr # x)
    (define (cxrIter s)
        (if (equal? s "")
            'x
            (if (equal? (car s) "a")
                (cons 'car (list (cxrIter (cdr s))))
                (cons 'cdr (list (cxrIter (cdr s))))
                )
            )
        )
    (eval (list 'lambda '(x) (cxrIter (string x))) #)
    )

(define (run8)
    (exprTest ((cxr 'adadd) (cons 1 (cons 2 (cons (cons 3 (cons 4 5)) 6)))) 4)
    )

;;; neuf
(include "table.scm")

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
    (putTable '+ '(STRING INTEGER) addStringInteger)
    (putTable '+ '(INTEGER STRING) addIntegerString)
    (putTable '- '(INTEGER STRING) subIntegerString)
    (putTable '- '(STRING INTEGER) subStringInteger)
    (putTable '* '(STRING INTEGER) mulStringInteger)
    (putTable '* '(INTEGER STRING) mulIntegerString)
    (putTable '/ '(INTEGER STRING) divIntegerString)
    'generic-system-installed
    )

(define (addStrings s1 s2)
    (string+ s1 s2)
    )

(define (addStringInteger s i)
    (string+ s (string i))
    )

(define (addIntegerString i s)
    (+ i (int s))
    )

(define (subIntegerString i s)
    (- i (int s))
    )

(define (subStringInteger s i)
    (if (= 0 i)
        s
        (subStringInteger (cdr s) (- i 1))
        )
    )

(define (mulStringInteger s i)
    (define (it x n)
        (if (= n 0)
            x
            (it (string+ x s) (- n 1))
            )
        )
    (if (= 0 i)
        ""
        (it "" i)
        )
    )

(define (mulIntegerString i s)
    (* i (int s))
    )

(define (divIntegerString i s)
    (/ i (int s))
    )


(define (uninstall-generic)
        (set! + old+)
        (set! - old-)
        (set! * old*)
        (set! / old/)
        'generic-system-uninstalled
        )

(define (apply-generic op x y)
  (define args (list x y))
  (define proc (getTable op (map type args)))
  (if (not (null? proc))
      (apply proc args)
      (cond
            ((equal? op '+)
                (apply old+ args))
            ((equal? op '-)
                (apply old- args))
            ((equal? op '*)
                (apply old* args))
            ((equal? op '/)
                (apply old/ args))
            )
          )
      )

(define (run9)
    (install-generic)
    (inspect (+ "x" "y"))
    (inspect (+ "123" 4))
    (inspect (+ 123 "4"))
    (inspect (- 123 "4"))
    (inspect (- "abc" 1))
    (inspect (* "abc" 3))
    (inspect (* 3 "33"))
    (inspect (/ 8 "2"))
    (uninstall-generic)
    )


;;; dix

(define (install-coercion)
    (clearTable)
    (putTable 'coerce '(REAL) real)
    (putTable 'coerce '(STRING) string)
    (putTable 'coerce '(INTEGER) int)
    'generic-system-installed
    )

(define (coerce n x) 
    (define proc (getTable 'coerce (list x)))
    (if (not (null? proc))
        (apply proc (list n))
        )
    )

(define (run10)
    (install-coercion)
  	(exprTest (coerce "123.4" 'INTEGER) 123)
	(exprTest (coerce '(1 (2.2) ((3 4) "5")) 'STRING) '(1 (2.2) ((3 4) "5")))
	(exprTest (type (coerce '(1 (2.2) ((3 4) "5")) 'STRING)) "STRING")
	)

(println "assignment 2 loaded!")
