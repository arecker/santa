(ql:quickload "cl-json")
(ql:quickload "cl-smtp")

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

(defun email-body (match config)
  (let ((gifter (first match))
	(giftee (second match)))
    (format nil
	    "Greetings ~A!

Your secret santa match is ~A.
Please mail your ~A gift to ~A by ~A.

Happy Holidays!

Santabot
https://github.com/arecker/santabot
"
	    (fullname gifter config)
	    (fullname giftee config)
	    (price config)
	    (address giftee config)
	    (deadline config))))

(defun mail-server (config)
  (assoc-val 'server (assoc-val 'mail config)))

(defun mail-user (config)
  (assoc-val 'user (assoc-val 'mail config)))

(defun mail-password (config)
  (assoc-val 'password (assoc-val 'mail config)))

(defun send (match config)
  (cl-smtp:send-email (mail-server config)
		      (mail-user config)
		      (email (first match) config)
		      "Your Secret Santa Match!"
		      (email-body match config)
		      :authentication `(,(mail-user config) ,(mail-password config)) :ssl :tls))

(defun main (configpath)
  (setf *config* (read-config configpath))
  (setf *possibilities* (possibilities *config*))
  (setf *matches* (pluck *possibilities*))
  (unless *matches*
    (error "No possible matches"))
  (mapcar (lambda (m) (send m *config*)) *matches*))

(main "~/git/santabot/example-config.json")
