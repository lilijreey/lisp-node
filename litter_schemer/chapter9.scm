;; Define pick 
(define pick
	(lambda (n lat)
			(cond 
				((zero? (- n 1)) (car lat))
				 (else (pick (- n 1) (cdr lat))))))
;; define keep-looking
(define keep-looking
	(lambda (a sorn lat)
			(cond
			  ((number? sorn)
			   (keep-looking a (pick sorn lat) lat))
			  (else
				(eq? sorn a)))))
;; define looking
(define looking
	(lambda (a lat)
			(keep-looking a (pick 1 lat) lat)))


;; What is pick define

;; from chapter 7

(DEFINE BUILD
	(LAMBDA (S1 S2)
			(CONS S1
				  (CONS S2 (QUOTE ())))))
;; DEFINE SHIFT
(DEFINE SHIFT
	(LAMBDA (PAIR)
			(BUILD (FIRST (FIRST PAIR))
				   (BUILD (SECOND (FIRST PAIR))
						  (SECOND PAIR)))))
;; Define atom? function
(define atom?
	(lambda (x)
			(and (not (pair? x))
				 (not (null? x)) ) ) )

;; Dfine length*
(define length*
	(lambda (pora)
			(cond
			  ((atom? pora) 1)
			  (else
				(+ (length* (first pora))
				   (length* (second pora)))))))

;; Defiend weight*
(define weight*
	(lambda (pora)
		    (cond
				 ((atom? pora) 1)
				 (else
				   (+ (* (weight* (first pora)) 2)
					  (weight* (second pora)))))))

;; Defien a-pair?
(define a-pair?
	(lambda (x)
			(cond
			  ((atom? x) #f)
			  ((null? x) #f)
			  ((null? (cdr x)) #f)
			  ((null? (cdr (cdr x))) #t)
			  (else #f))))

;; Define revpair that reversed the two components of a pair
(define revpair
	(lambda (pair)
			(build (second pair) (first pair))))

;; Define shuffle
(define shuffle
	(lambda (pora)
			(cond
			  ((atom? pora) pora)
			  ((a-pair? (first pora))
			   (shuffle (revpair pora)))
			  (else
				(build (first pora)
					   (shuffle (second pora)))))))

;; define implement the Collatz conjecture
(define C
	(lambda (n)
			(cond
				((= 1 n) (quote 1))
				(else
				  (cond
					((even? n) (C (/ n 2)))
					(else
					  (C (+ (* 3 n) 1))))))))

;; define wilhelm function
(define A
	(lambda (n m)
			(cond
			  ((zero? n) (+ m 1))
			  ((zero? m) (A (- n 1) n))
			  (else
				(A (- n 1)
				   (A n (- m 1)))))))

;; Defne infinite recursive
(define eternity
	(lambda (x)
			(eternity x)))


((lambda length
	(lambda (l)
			(cond
			  ((null? l) 0)
			  (else
				(+ (length (cdr l)) 1)))))
 eternity)

