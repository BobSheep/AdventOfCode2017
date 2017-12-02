(defun read-file (foo)
  (with-open-file (stream foo)
    (loop for line = (read-line stream nil)
       while line
       collect (with-input-from-string (in line)
		 (loop for x = (read in nil nil) while x collect x)))))

(defun maxmindiff (foo)
  (let ((mi (reduce #'min foo))
	(ma (reduce #'max foo)))
    (- ma mi)))

(defun descending (foo)
  (sort foo #'>))

(defun divisible (foo)
  (if (= (length foo) 1)
      0
      (+ (reduce  #'+
		  (map 'vector #'(lambda (x)
				   (if (= (mod (car foo) x) 0)
				       (/ (car foo) x)
				       0))
		       (cdr foo)))
	 (divisible (cdr foo)))))

(defun day2 (file)
  (let* ((bar (read-file file))
	 (prob1 (reduce #'+ (map 'vector #'maxmindiff bar)))
	 (prob2 (reduce #'+ (map 'vector #'(lambda (x) (divisible3 (descending x))) bar))))
    (list prob1 prob2)))

(day2 "day2.in")

