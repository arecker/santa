(defun read-config (path)
  (with-open-file (s path :direction :INPUT)
    (let ((json:*json-symbols-package* nil))
      (json:decode-json s))))

(defun shift (l)
  (append (last l)
	  (reverse (cdr (reverse l)))))

(defun match (l)
  (mapcar #'list l (shift l)))

(defun permute (l)
  (if (null l) '(())
      (mapcan #'(lambda (x)
		  (mapcar #'(lambda (y) (cons x y))
			  (permute (remove x l :count 1)))) l)))

(defun participants (config)
  (mapcar #'car (cdr (assoc 'participants config))))

(defun exclusions (name config)
  (mapcar (lambda (n) (intern (string-upcase n)))
	  (cdr (assoc 'exclusions
		      (cdr (assoc name
				  (cdr (assoc 'participants config))))))))

(defun allowed-match-p (match config)
  (let ((gifter (first match))
	(recipient (first (reverse match))))
    (= 0 (length (member recipient (exclusions gifter config))))))

(defun allowed-possibility-p (matches config)
  (every (lambda (m) (allowed-match-p m config)) matches))

(defun possibilities (config)
  (remove-if-not (lambda (p) (allowed-possibility-p p config))
		 (mapcar #'match (permute (participants config)))))

(defun pluck (l)
  (if l (nth (random (length l)) l) nil))
