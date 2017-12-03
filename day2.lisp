(defun read-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
       while line
       collect (with-input-from-string (in line)
		 (loop for x = (read in nil nil) while x collect x)))))

(defun maxmindiff (x)
  (let ((mi (reduce #'min x))
	(ma (reduce #'max x)))
    (- ma mi)))

(defun descending (sequence)
  (sort sequence #'>))

(defun divisible (sequence)
  (if (= (length sequence) 1)
      0
      (+ (reduce  #'+
		  (map 'vector #'(lambda (x)
				   (if (= (mod (car sequence) x) 0)
				       (/ (car sequence) x)
				       0))
		       (cdr sequence)))
	 (divisible (cdr sequence)))))

(defun day2 (file)
  (let* ((line (read-file file))
	 (prob1 (reduce #'+ (map 'vector #'maxmindiff line)))
	 (prob2 (reduce #'+ (map 'vector #'(lambda (x) (divisible (descending x))) line))))
    (list prob1 prob2)))

(day2 "day2.in")

