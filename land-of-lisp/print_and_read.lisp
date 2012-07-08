;; saying hello to the user
(defun say-hello ()
  (princ "Please type your name:")
  (let ((name (read)))
	   (princ "Nice to meet you, ")
	   (princ name) )
)

;;read and print a number instead of a stirng
(defun add-five ()
  (princ "please enter a number:")
  (let ((num (read)))
	   (princ "When I add five I get")
	   (princ (+ num 5)))
)
