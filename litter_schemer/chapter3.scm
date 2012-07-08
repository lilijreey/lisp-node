;; Define atom? function
(define atom?
	(lambda (x)
			(and (not (pair? x))
				 (not (null? x)) ) ) )

;; Define lat? function
(define lat?
	(lambda (l)
			(cond
			  ((null? l) #t)
			  ((atom? (car l)) (lat? (cdr l)))
			  (else #f) ) ) )

;; Define member? function
(define member?
	(lambda (a lat)
			(cond
			  ((null? lat) #f)
			  (else (or (eq? (car lat) a)
						(member? a (cdr lat)) ) ) ) ) )

;; Defien rember remove a member funciton
(define rember
	(lambda (a lat)
			(cond 
			  ((null? lat) (quote ()))
			  ((eq? (car lat) a) (cdr lat))
			  (else (cons (car lat)
						  (rember a (cdr lat)))))))

;; Define firsts get a list, builds another list composed of 
;; the first S-expressin of each internal list.
(define firsts
	(lambda (l)
			(cond
			  ((null? l) (quote ()))
			  (else
			  (cons (car (car l)) 
					(firsts (cdr l)))))))

;; Define insertR function :It takes thres arguments: the atomes new and old,
;; and a lat. The function insertR build a lat with new inserted to the
;; right of the firest occurrence of old.
(define insertR
	(lambda (new old lat)
			(cond
			  ((null? lat) (quote ()))
			   (else (cond
					   ((eq? (car lat) old)
						(cons old
							  (cons new (cdr lat))))
					   (else (cons (car lat)
								   (insertR 
									 new old (cdr lat)))))))))

;; Define insertL
(define insertL
	(lambda (new old lat)
			(cond 
			  ((null? lat) (quote ()))
			  (else (cond
					  ((eq? (car lat) old)
					   (cons new lat))
					  (else (cons (car lat)
								  (insertL new old (cdr lat)))))))))

;; multiinsertR 
(define multiinsertR
	(lambda (new old lat)
			(cond
			  ((null? lat) (quote ()))
			   (else (cond
					   ((eq? (car lat) old)
						(cons old
							  (cons new 
									(multiinsertR new old (cdr lat)))))
					   (else (cons (car lat)
								   (multiinsertR 
									 new old (cdr lat)))))))))

;; Define multiple insertL
(define multiinsertL (lambda (new old lat)
			(cond 
			  ((null? lat) (quote ()))
			  (else (cond
					  ((eq? (car lat) old)
					   (cons new 
							 (cons old
							      (multiinsertL new old (cdr lat)))))
					  (else (cons (car lat)
								  (multiinsertL new old (cdr lat)))))))))

;; Define subst funciton. It replaces the firest occurrence of 
;; old in the lat with new
(define subst
	(lambda (new old lat)
			(cond
			  ((null? lat) (quote ()))
			  (else (cond
					  ((eq? (car lat) old)
					   (cons new (cdr lat)))
					  (else (cons (car lat)
								  (subst new old (cdr lat)))))))))

;; Define stbst2. It is replaces either the first occurrence of o1 or o2
;; by new 
(define subst2
	(lambda (new o1 o2 lat)
			(cond
			  ((null? lat) (quote ()))
			  (else (cond
					  ((or (eq? o1 (car lat))
						   (eq? o2 (car lat)))
					  (cons new (cdr lat))) 
					 (else (cons (car lat)
								 (subst2 new o1 o2 (cdr lat)))))))))

;; define multirember function return the lat with all occurrences of aremoved.
(define multirember
	(lambda (a lat)
			(cond
			  ((null? lat) (quote ()))
			   (else
				 (cond 
				   ((eq? (car lat) a)
					(multirember a (cdr lat)))
				   (else (cons (car lat)
							   (multirember a 
											(cdr lat)))))))))
