;; REPL my interact 
(defun game-repl ()
  (loop (print (eval (read))))
)

;; create a top-level variable, *nodes*,  to contain descriptions
;; of the location that exist in our game

(defparameter *nodes* '( (living-room (you are in the living-room.
										   a wizard is snoring loudly on the couch.))
						 (garden (you are in a beautiful garden.
									  there is a well in front of you.))
						 (attic (you are in the attic.
									 there is a giant welding torch in the cornet.))
						)
)

;; create a describe-location function show defferent location describe
;; input location, nodes
(defun describe-location (location nodes)
  (cadr (assoc location nodes))
)

;; create edges this contains the paths that players can take to move 
;; between places on our map input: garden west door
(defparameter *edges* '( (living-room (garden west door)
									  (attic upstairs ladder))
						 (garden (living-room east door))
						 (attic (living-room downstairs ladder))
						)
)

;; search path 
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)
  )

;; Describing multiple paths at once
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges))))
)

;; List visible objects
(defparameter *objects* '(whiskey bucket frog chain))

;; location of each object 
(defparameter *object-locations* '( (whiskey living-room)
									(bucket living-room)
									(chain	garden)
									(frog garden) )
)

;; a fucntion that lists the objects visible from given location
;; loc is you want location of find object
;; objs is list of objects
;; obj-locs is *object-location*
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (o
			 (eq (cadr (assoc obj obj-locs)) loc) ))
		  (remove-if-not #'at-loc-p objs)))
)

;; describing visible objects
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
			 `(you see a ,obj on the floor.) ))
	(apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc))))
)

;;set player's current position
(defparameter *location* 'living-room)

;;show all of these description of location
(defun look ()
  (append (describe-location *location* *nodes*)
		  (describe-paths *location* *edges*)
		  (describe-objects *location* *objects* *object-locations*) )
)
  
;; takes a direction and lets us walk there
(defun walk (direction)
  (let ((next (find direction
					(cdr (assoc *location* *edges*))
					:key #'cadr)))
	   (if next
		   (progn (setf *location* (car next))
				  (look))
		   '(you cannot go that way.)))
)
 
;; pick up objects in our world.
(defun pickup (object)
  (cond ( (member object
				 (objects-at *location* *objects* *object-locations*))
		  (push (list object 'boby) *object-locations*)
		  `(you are now carrying the ,object) )
		(t '(you cannot get that.) ) ) 
)
  
;; checking our inventory
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*))
)
