;; Exercise 2.2.1
;a)
(define a-procedure
	(+ (* 1.2
		  (- 2 1/3))
	   -8.7))
;b)
(define b-procedure
	(/ (+ 2/3 4/9)
	   (- 5/11 4/3))) 
;c)
(define c-procedure
	(+ 1
	   (/ 1
		   (+ 2
			  (/ 1
				 (+ 1 1/2))))))
;d)
(define d-procedure
	(* 1 -2 3 -4 5 -6 7))

;; exercise 2.2.5
(define s 
	(list (cons 'a 'b) (cons (list 'c) (list 'd)) '()))

;; exercise 2.4.1
(let ([x (* 3 a)])
	 (+ (- x b)
		(+ x b)))
(let ([l (list a b c)])
	 (cons (car l) (cdr l)))

;; exercise 2.4.3
;a)
(let ([x 'a] [y 'b])
	 (list (let ([inter-x 'c]) (cons inter-x y))
		   (let ([inter-y 'd]) (cons x inter-y))))
;b)
(let ([x0 '((a b) c)])
	 (cons (let ([x1 (cdr x0)])
				(car x1))
		   (let ([x2 (car x0)])
				(cons (let ([x3 (cdr x2)])
						   (car x3))
					  (cons (let ([x4 (car x3)])
								 x)
							(cdr x2))))))

;; exericse 2.6.2
(define compose
	(lambda (x y)
			(lambda (l)
					(x (y l)))))

(define (cadr l)
	((compose car cdr) l))
(define (cdar l)
	((compose cdr car) l))

;; exercise 2.7.2
(define (atom? x)
	(if (pair? x)
		#f
		#t))
(define atom?
	(lambda (x)
			(not (pair? x))))

(define shorter
	(lambda (l1 l2)
			(let ([len1 (length l1)]
				  [len2 (length l2)])
				 (if (>= len1 len2)
					 l2
					 l1))))

(define shorter
	(lambda (l1 l2)
			(if (< (length l2) (length l1))
				l2
				l1)))

;; exerice 2.8.3
(define make-list
	(lambda (n o)
			(if (= 0 n)
				'()
				(cons o (make-list (- n 1) o)))))

;; exerice 2.8.4
(define list-ref
	(lambda (l n)
			(if (= n 0)
				(car l)
				(list-ref (cdr l) (- n 1)))))

(define list-tail
	(lambda (l n)
			(if (= n 0)
				l
				(list-tail (cdr l) (- n 1)))))

;; exerice 2.8.5
(define shorter?
	(lambda (l1 l2)
			(cond ((null? l1) #t)
				  ((null? l2) #f)
				  (else
					(shorter? (cdr l1) (cdr l2))))))
(define shorter
	(lambda (l1 l2)
			(if (shorter? l1 l2)
				l1
				l2)))

;; exercise 2.8.6
(define even? 
	(lambda (x)
			(if (= 0 x)
				#t
				(odd? (- x 1)))))
(define odd?
	(lambda (x)
			(if (= 0 x)
				#f
				(even? (-x 1)))))

;; exercise 2.8.7
(define transpose 
	(lambda (l)
			(cons (map car l1) (map cdr l2))))

;; exercise 2.9.1
(define make-counter
	(lambda (init incre)
			(let ([next init])
				 (lambda ()
						 (let ([v next])
							  (set! next (+ next incre))
							  v)))))

;; exercise 2.9.3
	(cond (((eqv? msg 'ref) (list-ref ls var))
		   ((eqv? msg 'set!) (set-car! var (list-tail )))))

;; exercise 2.9.4
(define make-starck
	(lambda (n)
			(let ([ls (vector n)])
				 (lambda (msg . args)
						 (case msg
							   [(empty?) (null? ls)]
							   [(push?) (set! ls (cons (car args) ls))]
							   [(pop?) (set! ls (cdr ls))]
							   [(top) (car ls)]
