;; atom?
(define atom?
	(lambda (x)
			(and (not (pair? x))
				 (not (null? x)) )))

;; Define rember* remove spcisfy atome from a list
(define rember*
	(lambda (a l)
			(cond 
			  ((null? l) (quote () ))
			  (else (cond
					  ((atom? (car l))
							  (cond 
								((eq? a (car l))
								 (rember* a (cdr l)))
								(else (cons (car l) 
											(rember* a (cdr l))))))
					  (else (cons (rember* a (car l))
								  (rember* a (cdr l)))))))))

;; Define insertR* 
(define insertR*
	(lambda (new old l)
			(cond
			  ((null? l) (quote ()))
			  ((atom? (car l))
			   (cond
				 ((eq? old (car l)) 
				  (cons old (cons new 
								  (insertR* new old (cdr l)))))
				 (else 
				   (cons (car l) (insertR* new old (cdr l))))))
			  (else 
				(cons (insertR* new old (car l))
					  (insertR* new old (cdr l)))))))

;; Define insertL*
(define insertL*
	(lambda (new old l)
			(cond
			  ((null? l) (quote ()))
			  ((atom? (car l))
			   (cond
				 ((eq? old (car l)) 
				  (cons new (cons old
								  (insertL* new old (cdr l)))))
				 (else 
				   (cons (car l) (insertL* new old (cdr l))))))
			  (else 
				(cons (insertL* new old (car l))
					  (insertL* new old (cdr l)))))))
;; test
(insertL* 'roast 'chuck
		  '((how much (wood))
			could ((a (wood) chuck))
			(((chuck))) (if (a) ((wood chuck)))
			could chuck wood))

;; Define occur*
(define occur*
	(lambda (a l)
			(cond 
			  ((null? l) 0)
			  ((atom? (car l))
			   (cond
				 ((eq? a (car l))
				  (+ 1 (occur* a (cdr l))))
				 (else
				   (occur* a (cdr l)))))
			  (else
				(+ (occur* a (car l))
				   (occur* a (cdr l)))))))

;; Defien subst*
(define subst*
	(lambda (new old l)
			(cond 
			  ((null? l) (quote ()))
			  ((atom? (car l))
			   (cond
				 ((eq? old (car l))
				  (cons new (subst* new old (cdr l))))
				 (else
				   (cons (car l)
						 (subst* new old (cdr l))))))
			  (else
				(cons (subst* new old (car l))
				      (subst* new old (cdr l)))))))
			
;; Defien member*
(define member*
	(lambda (a l)
			(cond 
			  ((null? l) #f)
			  ((atom? (car l)
					  (cond
						((eq? (car l) a)
						 #t)
						(else
						  (member (a (car l))))))
			   (else (or (member* a (car l))
						 (member* a (cdr l))))))))

;; Defien eqlist?. It detemine if two list same
(define eqlist?
	(lambda (l1 l2)
			(cond
			  ((and (null? l1) (null? l2)) #t)
			  ((or (null? l1) (null? l2)) #f)
			  ((and (atom? (car l1))
					(atom? (car l2)))
			   (and (eqan? (car l1) (car l2))
					(eqlist? (cdr l1) (cdr l2))))
			  ((or (atom? (car l1))
				   (atom? (car l2)))
			   #f)
			  (else
				(and (eqlist? (car l1) (car l2))
					 (eqlist? (cdr l1) (cdr l2)))))))

(define equal? 
	(lambda (s1 s2)
			(cond
			  ((and (atom? s1) (atom? s2))
			   (eqan? s1 s2))
			  ((or (atom? s1) (atom? s2))
			   #f)
			  (else (eqlist? s1 s2)))))

;; rewrite eqlsit? use equal?
(define eqlist?
	(lambda (l1 l2)
			(cond
			  ((and (null? l1) (null? l2)) #t)
			  ((or (null? l1) (null? l2)) #f)
			  (else
				(and (equal? (car l1) (car l2))
					 (equal? (cdr l1) (cdr l2)))))))
