;; TODO: Get this stuff into JSON or something
(defparameter *participants* '(alex marissa ben
			       beth adam jordan
			       joel tanya eric
			       lauren chi caleb))

(defparameter *exclusions* '((alex (marissa) )
			     (marissa (alex))
			     (tanya (joel))
			     (joel (tanya))
			     (ben (beth))
			     (beth (ben))
			     (lauren (eric))
			     (eric (lauren))
			     (adam (jordan))
			     (jordan (adam))))

(defun shuffle-list (sequence)
  (loop for i from (length sequence) downto 2
	do (rotatef (elt sequence (random i))
		    (elt sequence (1- i))))
  sequence)

(defun shift-list (list)
  (append (cdr list) (list (car list))))

(defun zip-lists (a b)
  (mapcar #'list a b))

(defun forbidden-match-p (pair exclusions)
  (let* ((gifter (car pair))
	 (giftee (car (last pair))))
    (member giftee (first (cdr (assoc gifter exclusions))))))

(defun match (participants)
  (let ((shifted-participants (shift-list participants)))
    (zip-lists participants shifted-participants)))

(defun print-match (pair)
  (format t "~A -> ~A~%" (car pair) (car (last pair))))

(defun main ()
  (setf *keep-shaking* t)
  (loop while *keep-shaking* do
       (let ((shuffled-participants (shuffle-list *participants*)))
	 (setf *matched-participants* (remove-if (lambda (p) (forbidden-match-p p *exclusions*))
						 (match shuffled-participants)))
	 (setf *keep-shaking* (not (equal (list-length shuffled-participants)
					  (list-length *matched-participants*))))))
  (mapcar #'print-match *matched-participants*))
