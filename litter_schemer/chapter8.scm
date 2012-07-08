;; Define atom? function
(define atom?
	(lambda (x)
			(and (not (pair? x))
				 (not (null? x)) ) ) )
;; Define eq?-c
(define eq?-c
	(lambda (a)
			(lambda (x)
					(eq? x a))))

;; Defien eq?-salado
(define eq?-salad (eq?-c 'salad))

;; Define rember-f
(define rember-f
	(lambda (test?)
			(lambda (a l)
					(cond
					  ((null? l) (quote ()))
					  ((test? (car l) a) (cdr l))
					  (else
						(cons (car l) 
							  ((rember-f test?) a 
							   (cdr l))))))))

;;Define rember-eq?
(define rember-eq? (rember-f eq?))

;; Define insertL-f the same way rember-f
(define insertL-f
	(lambda (test?)
			(lambda (new old l)
					(cond
					  ((null? l) (quote ()))
					  ((test? (car l) old)
					   (cons new (cons old
									   (cdr l))))
					  (else
						(cons (car l)
							  ((insertL-f test?) new old (cdr l))))))))
						
						
;; Defien insertR-f
(define insertR-f
	(lambda (test?)
			(lambda (new old l)
					(cond 
					  ((null? l) (quote ()))
					  ((test? (car l) old)
					   (cons old (cons new 
									   (cdr l))))
					  (else
						(cons (car l)
							  ((insertR-f test?) new old (cdr l))))))))

;; Define insert-g that would inserteither at the left or at the right
;; the direction is l or r
(define insert-g
	(lambda (direction)
			(lambda (new old l)
					(cond
					  ((null? l) (quote ()))
					  ((eq? (car l) old)
					   (cond
						 ((eq? direction 'l)
						  (cons new (cons old
										  (cdr l))))
						 ((eq? direction 'r)
						  (cons old (cons new
										  (cdr l))))
						 (else
						   l)))
					  (else
						(cons (car l)
							  ((insert-g direction) new old (cdr l))))))))


;; Defien seqR, and seqL
(define seqR
	(lambda (new old l)
			(cons old (cons new l))))
(define seqL
	(lambda (new old l)
			(cons new (cons old l))))

;; rewrite insert-g with seqL or seqR
(define insert-g
	(lambda (seq)
			(lambda (new old l)
					(cond
					  ((null? l) (quote ()))
					  ((eq? (car l) old)
					   (seq new old (cdr l)))
					  (else
						(cons (car l)
							  ((insert-g seq) new old (cdr l))))))))

;; Define insertL and insertR
(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

;; redefien insertL and insertR
(define insertL
	(insert-g
	  (lambda (new old l)
			  (cons new (cons old l)))))

;; define seqS
(define seqS
	(lambda (new old l)
			(cons new l)))

;; Define subst using insert-g
;;define 通常有宏的功能
(define subst (insert-g seqS))

(define seqrem
	(lambda (new old l)
			l))
(define yyy
	(lambda (a l)
			((insert-g seqrem) 'adfefe a l)))

;; define that takes one arguemt x and return the function opreater
(define atom-to-function
	(lambda (x)
			(cond
			  ((eq? x (quote +)) +)
			  ((eq? x (quote *)) *)
			  (else 
				-))))
(define value
	(lambda (nexp)
			(cond
			  ((atom? nexp) nexp)
			  (else
				((atom-to-function (car nexp))
				  (value (car (cdr nexp)))
				  (value (car (cdr (cdr nexp)))))))))
				  

;; Defien multirember-f
(define multirember-f
	(lambda (test?)
			(lambda (a lat)
					(cond
					  ((null? lat) (quote ()))
					  ((test? a (car lat))
					   ((multirember-f test?) a (cdr lat)))
					  (else
						(cons
						  (car lat)
						  ((multirember-f test?) a (cdr lat))))))))

;; define multirember-eq? using multirember-f
(define multirember-eq? (multirember-f eq?))

(define eq?-tuna
	(eq?-c 'tuna))

(define multirember-T
	(lambda (test? lat)
					(cond
					  ((null? lat) (quote ()))
					  ((test?  (car lat))
					   ((multirember-T test? (cdr lat)))
					  (else
						(cons
						  (car lat)
						  ((multirember-T test? (cdr lat)))))))))

;; Define multirember-co
(define multirember-co
	(lambda (a lat col)
			(cond
			  ((null? lat)
			   (col (quote ()) (quote ())))
			  ((eq? (car lat) a)
			   (multirember-co a
							   (cdr lat)
							   (lambda (newlat seen)
									   (col newlat
											(cons (car lat) seen)))))
			  (else
				(multirember-co a
								(cdr lat)
								(lambda (newlat seen)
										(col (cons (car lat) newlat)
											 seen)))))))

(define a-friend
	(lambda (x y)
			(null? y)))


;; Define new-friend
(define new-friend
	(lambda (newlat seen)
			(col newlat
				 (cons (car lat) seen))))
;; redefine with a-friend 
(define new-friend
	(lambda (newlat seen)
			(a-friend newlat
						(cons (quote tuna) seen))))

(define latest-friend
	(lambda (newlat seen)
			(a-friend (cons (quote and) newlat)
					  seen)))
;; show the multirember-co anwser
(define show-friend
	(lambda (a b)
			(cons a (cons '. b))))

;; Define multiinsertLR that inserts new to the left of oldL and 
;; the right of oldR in lat. if oldL are oldR are different.
(define multiinsertLR
	(lambda (new oldL oldR lat)
			(cand
			  ((null? lat) (quote ()))
			  ((eq? (car lat) oldL)
			   (cons new
					 (cons oldL
						   (multiinsertLR new oldL oldR (cdr lat)))))
			  ((eq? (car lat) oldR)
			   (cons oldR
					 (cons new
						   (multiinsertLR new oldL oldR (cdr lat)))))
			  (else
				(cons (car lat)
					  (multiinsertLR new oldL oldR (cdr lat)))))))


;; Define multiinsertLR-cOo
(define add1
	(lambda (x)
			(+ x 1)))

(define multiinsertLR-co
	(lambda (new oldL oldR lat col)
			((null? lat) 
			 (col (quote ()) 0 0))
			((eq? (car lat) oldL)
			 (multiinsertLR-co new oldL oldR (cdr lat)
							   (lambda (newlat L R)
									   (col (cons new
												  (cons oldL newlat))
											(addl1 L) R))))
			((eq? (car lat) oldR)
			 (multiinsertLR-co new oldL oldR (cdr lat)
							   (lambda (newlat L R)
									   (col (cons oldR
												  (cons new newlat))
											L (add1 R)))))
			(else
			  (multiinsertLR-co new oldL oldR (cdr lat)
								(lambda (newlat L R)
										(col (cons (car lat)
												   newlat)
											 L R))))))
(define three
	(lambda (newlat L R)
			(cons newlat (cons L R))))
(define fun
	(lambda (newlat L R)
			(null? newlat)))
  
  

(define even?
	(lambda (n)
			(= (* (/ n 2) 2) n)))

(define evens-only*
	(lambda (l)
			(cond
			  ((null? (quote ())))
			  ((atom? (car l))
			   (cond
				((even? (car l))
				 (cons (car l)
					   (evens-only* (cdr l))))
				 (else
				   (evens-only* (cdr l)))))
			  (else
				(cons (evens-only* (car l))
					  (evens-only* (cdr l)))))))
