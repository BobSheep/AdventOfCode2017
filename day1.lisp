(defun read-file (file)
  (with-open-file (stream file)
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

(defun opposite-equal-sum (sequence)
  (let* ((offseq (nthcdr (/ (length sequence) 2) sequence)))
    (* 2 (reduce #'+ (map 'vector #'val-if-equal sequence offseq)))))


(defun day1 (in-file)
  (let* ((input (number-to-list (caar (read-file in-file))))
	 (part1 (adjacent-equal-sum (first input) input))
	 (part2 (opposite-equal-sum input)))
    (list part1 part2)))

(day1 "day1.in")
