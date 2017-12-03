(defun read-file (foo)
  (with-open-file (stream foo)
    (loop for line = (read-line stream nil)
       while line
       collect (with-input-from-string (in line)
		 (loop for x = (read in nil nil) while x collect x)))))

(defun number-to-list (n)
  (loop for c across (write-to-string n) collect (digit-char-p c)))

(defun val-if-equal (x y)
  "Returns value if equal"
  (if (= x y)
      x
      0))

(defun adjacent-equal-sum (first sequence)
  ""
  (if (= (length sequence) 1)
      (val-if-equal first (car sequence))
      (+ (val-if-equal (car sequence) (cadr sequence))
	 (adjacent-equal-sum first (cdr sequence)))))

(let* ((input (number-to-list (caar (read-file "day1.in")))))
  (adjacent-equal-sum (first input) input))

