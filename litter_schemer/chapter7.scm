(define atom?
	(lambda (x)
			(and (not (pair? x))
				 (not (null? x)) ) ) )

;; Define member? function
(define member?
	(lambda (a lat)
			(cond
			  ((null? lat) #f)
			  (else (or (eq? (car lat) a)
						(member? a (cdr lat)) ) ) ) ) )

;; Define set. Test the atom of lat no more than once.
(define set?
	(lambda (lat)
			(cond
			  ((null? lat) #t)
			  ((member? (car lat) (cdr lat)) #f)
			  (else
				(set? (cdr lat))))))

;; Define makeset,	It take the member of lat at most once.
;; use member?
(define makeset
	(lambda (lat)
			(cond
			  ((null? lat) (quote ()))
			  ((member? (car lat) (cdr lat))
			   (makeset(cdr lat)))
			  (else
				(cons (car lat) 
					  (makeset (cdr lat)))))))
				
;; Use multirember rewrite makeset
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
(define makeset
	(lambda (lat)
			(cond
			  ((null? lat) (quote ()))
			  (else
				(cons (car lat)
					(makeset (multirember (car lat) 
										  (cdr lat))))))))

;; Define subset?. test if set1 is subset of set2. each atom in set1 is also
;; in set2
(define subset?
	(lambda (set1 set2)
			(cond
			  ((null? set1) #t)
			  ((member? (car set1) set2)
			   (subset? (cdr set1) set2))
			  (else #f))))

;; rewrite subset? with (and ...)
(define subset?
	(lambda (set1 set2)
			(cond
			  ((null? set1) #t)
			  (else
				(and (member? (car set1) set2)
					 (subset? (cdr set1) set2))))))

;; Define eqset?. It test two lat that if echo atom in one lat is alos in other. 
(define eqset?
	(lambda (set1 set2)
			(cond
			  ((subset? set1 set2)
			   (subset? set2 set1))
			  (else
				#f))))

;; rewrite eqset?
(define eqset?
	(lambda (set1 set2)
			(cond
			  (else
				(and (subset? set1 set2)
					 (subset? set2 set1))))))

;; rewirte eqset? wihe one-liner.
(define eqset?
	(lambda (set1 set2)
			(and (subset? set1 set2)
				 (subset? set2 set1))))

;; 交集 intersecton. Define intersect?
(define intersect?
	(lambda (set1 set2)
			(cond
			  ((null? set1) #f)
			  ((member (car set1) set2)
			   #t)
			  (else
				(intersect? (cdr set1)
							set2)))))
;; rewrite intersect? 
(define intersect?
	(lambda (set1 set2)
			(cond
			  ((null? set1) #f)
			  (else
				(or (member (car set1) set2)
					(intersect? (cdr set1) set2))))))


;; Define intersect
(define intersect
	(lambda (set1 set2)
			(cond
			  ((null? set1) (quote ()))
			  ((member (car set1) set2)
			   (cons (car set1)
					 (intersect (cdr set1) set2)))
			  (else
				 (intersect (cdr set1) set2)))))

;; union 并集; Define union
(define union
	(lambda (set1 set2)
			(cond
			  ((null? set1)
			   set2)
			 (else
			   (union (cdr set1) 
					  (makeset (cons (car set1)
									 set2)))))))
;; rewrite union
(define union
	(lambda (set1 set2)
			(cond
			  ((null? set1) set2)
			  ((member? (car set1) set2)
			   (union (cdr set1) set2))
			  (else
				(cons (car set1)
					  (union (cdr set1) set2))))))


;; Define intersectall. It get all of intersect
(define intersectall
	(lambda (set)
			(cond
			  ((null? (cdr set))
			   (car set))
			  (else
				(intersect (car set) 
						  (intersectall (cdr set)))))))

;; Define first
(define first
	(lambda (p)
			(car p)))
(define second 
	(lambda (p)
			(car (cdr p))))
(define third 
	(lambda (p)
			(car (cdr (cdr p)))))

(define build
	(lambda (s1 s2)
			(cons s1
				  (cons s2 (quote ())))))

;; from Chapter3
;; Define firsts get a list, builds another list composed of 
;; the first S-expressin of each internal list.
(define firsts
	(lambda (l)
			(cond
			  ((null? l) (quote ()))
			  (else
			  (cons (car (car l)) 
					(firsts (cdr l)))))))

;; Define fun? withe set? and firsts. test the firsts of rel is or is not a set.
(define fun?
	(lambda (rel)
			(set? (firsts rel))))

;; Defien revrel that it is consionver every theelements of pairs oflist
(define revrel 
	(lambda (rel)
			(cond
			  ((null? rel)
			   (quote ()))
			  (else
				(cons (build 
						(second (car rel))
						(first (car rel)))
					  (revrel (cdr rel)))))))

;; rewrite revrel
(define revrel
	(lambda (rel)
			(cond
			  ((null? rel) (quote ()))
			  (else
				(cons
				  (cons (car (cdr (car rel)))
						(cons (car (car rel))
							  (quote ())))
				  (revrel (cdr rel)))))))

;; Define revpair that reversed the two components of a pair
(define revpair
	(lambda (pair)
			(build (second pair) (first pair))))

;; rewrite revrel with revpair
(define revrel
	(lambda (rel)
			(cond
				((null? rel) (quote ()))
				(else
				  (cons (revpair (car rel))
						(revrel (cdr rel)))))))

;; Define seconds that get the second of pair of l
(define seconds
	(lambda (rel)
			(cond
			  ((null? rel) (quote ()))
			  (else
				(cons (second (car rel))
					  (seconds (cdr rel)))))))

;; Define fullfun? that test if second of pair of rel is set
(define fullfun?
	(lambda (fun)
			(set? (seconds fun))))
