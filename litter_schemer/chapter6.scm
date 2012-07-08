;; atom?
(define atom?
	(lambda (x)
			(and (not (pair? x))
				 (not (null? x)) )))

;; Define numbered? . It test if aexp is arithmetic expression in list form
;; An arithmetic expression is either a number,  or two arithmetic expressions
;; combined by +,  X, ^(power) e.g (2 + (2 + (3 + 1)))
;; attation: the difference between 'cdr' and 'car'
;; e.g (car '(1 + 2)) 1
;;     (cdr '(1 + 2)) (+ 2)
               
(define numbered?
	(lambda (aexp)
			(cond 
			  ((atom? aexp)
			   (number? aexp))
			  ((eq? (car (cdr aexp)) (quote +))
			   (and (numbered? (car aexp))
					(numbered? (car (cdr (cdr aexp))))))
			  ((eq? (car (cdr aexp)) (quote *))
			   (and (numbered? (car aexp))
					(numbered? (car (cdr (cdr aexp))))))
			  ((eq? (car (cdr aexp)) (quote -))
			   (and (numbered? (car aexp))
					(numbered? (car (cdr (cdr aexp))))))
			  (else
				#f))))
			   
(define numbered?
	(lambda (aexp)
			(cond 
			  ((atom? aexp) (number? aexp))
			  (else
				(and (numbered? (car aexp))
					 (numbered? (car (cdr (cdr aexp)))))))))

;; Define value is return what the value of a numbered arithmetic expression.
;; the argument must be a arithmetic expression
(define value
	(lambda (nexp)
			(cond
			  ((atom? nexp) nexp)
			  ((eq? (car (cdr nexp)) (quote +))
			   (+ (value (car nexp))
				  (value (car (cdr (cdr nexp))))))
			  ((eq? (car (cdr nexp)) (quote *))
			   (* (value (car nexp))
				  (value (car (cdr (cdr nexp))))))
			  ((eq? (car (cdr nexp)) (quote -))
			   (- (value (car nexp))
				  (value (car (cdr (cdr nexp))))))
			  (else
				0))))

;; Now wo define a value that it is arithmetic 逆波兰 expression
;; e.g (+ 3 (- 4 1))
(define value1
	(lambda (nexp)
			(cond
			((atom? nexp) nexp)
			((eq? (car nexp) (quote +))
			 (+ (value1 (car (cdr nexp))) 
				(value1 (car (cdr (cdr nexp))))))
			((eq? (car nexp) (quote *))
			 (* (value1 (car (cdr nexp))) 
				(value1 (car (cdr (cdr nexp))))))
			((eq? (car nexp) (quote -))
			 (- (value1 (car (cdr nexp))) 
				(value1 (car (cdr (cdr nexp))))))
			(else
			  0))))
