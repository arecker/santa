(ql:quickload "cl-json")

(defun read-config (path)
  (with-open-file (s path :direction :INPUT)
    (let ((json:*json-symbols-package* nil))
      (json:decode-json s))))

(defun assoc-val (item alist)
  (cdr (assoc item alist)))

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
  (mapcar #'car (assoc-val 'participants config)))

(defun exclusions (name config)
  (mapcar (lambda (n) (intern (string-upcase n)))
	  (assoc-val 'exclusions
		     (assoc-val name
				(assoc-val 'participants config)))))

(defun allowed-match-p (match config)
  (let ((gifter (first match))
	(recipient (first (reverse match))))
    (= 0 (length (member recipient (exclusions gifter config))))))

(defun allowed-possibility-p (matches config)
  (every (lambda (m) (allowed-match-p m config)) matches))

(defun possibilities (config)
  (remove-duplicates
   (mapcar (lambda (p) (sort p #'string<= :key #'first))
	   (remove-if-not (lambda (p) (allowed-possibility-p p config))
			  (mapcar #'match (permute (participants config)))))
   :test #'equal))

(defun pluck (l)
  (if l (nth (random (length l)) l) nil))

(defun fullname (name config)
  (assoc-val 'name (assoc-val name (assoc-val 'participants config))))

(defun address (name config)
  (assoc-val 'address (assoc-val name (assoc-val 'participants config))))

(defun email (name config)
  (assoc-val 'email (assoc-val name (assoc-val 'participants config))))

(defun price (config)
  (assoc-val 'price config))

(defun deadline (config)
  (assoc-val 'deadline config))

;; (defun email-body (gifter recipient config)
;;   (format nil "Greetings %s!" (fullname gifter config)))

(defun main (configpath)
  (pluck (possibilities (read-config configpath))))

(pprint (main "~/git/santabot/example-config.json"))
