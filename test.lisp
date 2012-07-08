;;;(defun atom-count (x)
;; "null is not atoms" (cond ((null x) 0)
;;	((atom x) 1)
;;	(t (+ (atom-count (first x))
;;	      (atom-count (rest x))))))
;;   
;;
;;
;;(defun atom-all-count (x)
;; "null is atom"
;; (cond ((null x) 1)
;;       ((atom x) 1)
;;	(t (+ (atom-count (first x))
;;	      (atom-count (rest x))))))
;;
;;    
;;
;;(defun dot-product (a b)
;;    (apply #'+ (mapcar #'* a b)))
;;
;;     
;;(setf x '(2 3 4))
;;(first x)
   
(defun of1(ll)
 ;; function that return a number which great than 1 
 ;; and less than 5
 (mapcar #'(lambda (n) 
	    (and (numberp n) (> n 1) (< n 5))) ll))

(defun of2(ll)
 ;; as same as of1 but use remove-if-not
 (remove-if-not #'(lambda (n) 
		    (and (numberp n) 
		         (> n 1) 
			 (< n 5))) 
		ll))
