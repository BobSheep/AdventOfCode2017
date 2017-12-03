(defun odd-sqrt (x)
  (+ 1 (* 2 (floor (/ (- (isqrt x) 1 )2)))))

(defun d3-part1 (x)
  (let* ((ps (odd-sqrt x))
	 (diff (- x (expt ps 2)))
	 (len (+ ps 2))
	 (from-corner (mod diff (- len 1)))
	 (mid (ceiling (/ len 2)))
	 (from-mid (- mid (abs (- (floor (/ len 2)) from-corner))))
	 (steps (- len from-mid)))
    steps))

(d3-part1 361527)
