;; Define add1 function. It add 1 to argument
(define add1
	(lambda (n)
			(+ n 1)))

;; Define sub1 function. It sub 1 from argument
(define sub1
	(lambda (n)
			(- n 1)))

;; There are no  negative number.
;; Define o+ symbol that add two argument
(define o+
	(lambda (x y)
			(cond
			  ((zero? y) x)
			  (else (add1 (o+ x (sub1 y)))))))

;; Define o- symbol that sub two argumetn
(define o-
	(lambda (x y)
			(cond 
			  ((zero? y) x)
			  (else (sub1 (o- x (sub1 y)))))))

;; Define o* 
(define o*
	(lambda (x y)
			(cond 
			  ((zero? y) 0)
			  (else (o+ x 
						(o* (sub1 y)))))))

;; Define addtup,  It builds a muber by totaling all the numbers in a tuple
(define addtup
	(lambda (tup)
			(cond
			  ((zero? tup) 0)
			  (else (+ (car tup) 
					   (addtup (cdr tup)))))))

;; Define tup+. It adds the first number of tup1 to the first number of tup2
;; , and so on, building a tup of the answers. 
(define tup+ 
	(lambda (tup1 tup2)
		   (cond 
			 ((null? tup1) tup2)
			 ((null? tup2) tup1)
			 (else (cons (+ (car tup1) (car tup2))
						 (tup+ (cdr tup1) (cdr tup2)))))))

;; Define pick 
(define pick
	(lambda (n lat)
			(cond 
				((zero? (sub1 n)) (car lat))
				 (else (pick (sub1 n) (cdr lat))))))

;; Define rempick from first to n
(define rempick
	(lambda (n lat)
			(cond 
			  ((zero? (sub1 n)) (cdr lat))
			  (else (cons (car lat) 
						  (rempick (sub1 n) (cdr lat)))))))

;; Define no-nums get the removing all the numbers from the lat.
(define no-nums
	(lambda (lat)
			(cond
			  ((null? lat) (quote ()))
			  (else (cond
						 ((number? (car lat)) 
						  (no-nums (cdr lat)))
						(else (cons (car lat) 
									(no-nums (cdr lat))))))))) 

;; Define all-nums which extracts a tup from a lat using all the number in the lat
(define all-nums
	(lambda (lat)
			(cond 
			  ((null? lat) (quote ()))
			  (else 
				(cond
				  ((number? (car lat))
					 (cons (car lat)
						   (all-nums (cdr lat))))
				  (else 
					(all-nums (cdr lat))))))))

;; Define eqan? which is true if ites two arguemts are the same atom.
(define eqan? 
	(lambda (a1 a2)
			(cond
			  ((and (number? a1)
					 (number? a2))
			   (= a1 a2))
			  ((or (number? a1)
					(number? a2))
			   #f)
			  (else (eq? a1 a2)))))

;; Define occur which counts the number of times an atom appears in lat
(define occur
	(lambda (a lat)
			(cond 
			  ((null? lat) 0)
			  (else (cond
					  ((eq? a (car lat))
					   (add1 (occur a (cdr lat))))
					  (else (occur a (cdr lat)))))))) 

;; Define rempick
