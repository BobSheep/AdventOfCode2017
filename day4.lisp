(defun read-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
       while line
       collect (with-input-from-string (in line)
		 (loop for x = (read in nil nil) while x collect x)))))

(defun compare-strings (sorted)
  (member (car sorted) (cdr sorted) :test #'string=))

(defun compare-symbols (symbol)
  (member (car symbol) (cdr symbol)))

(defun compare-anagram (strings)
  (compare-strings (sort-strings strings)))

(defun sort-strings (strings)
  (map 'list #'(lambda(x)(sort x #'string> )) (map 'list #'string strings)))

(defun count-valid (list)
  (count-if #'(lambda(x) x) list))

(defun is-valid (func passphrase)
  (reduce #'(lambda (x y)(and x y)) (maplist #'(lambda (x) (if (funcall func x) nil t)) passphrase)))

(defun day4 (in-file)
  (let ((input (read-file in-file)))
    (list (count-valid (map 'vector #'(lambda(x)(is-valid #'compare-symbols x)) input))
	  (count-valid (map 'vector #'(lambda(x)(is-valid #'compare-anagram x)) input)))))

(day4 "day4.in")
