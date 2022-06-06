;;;; test 03-string-utilities
;;;;

(let* ((original "foo")
       (copy (clone original)))
  (pass? "string clone 3.1" (eq original copy)))

(let ((test-string "ABC"))
  (pass? "final 3.2" (string= (final test-string) "C"))
  (pass? "butfinal 3.3" (string= (butfinal test-string) "AB"))
  (pass? "string cnth 3.4" (and (string= "B" (cnth 1 test-string))
			    (string= "B" (cnth 4 test-string))))
  (pass? "str+ 3.5" (string= (str+ "AB" "CD") "ABCD"))
  (pass? "spaces 3.6" (string= "    " (spaces 4)))
  (pass? "scopies 3.7" (string= "..." (scopies 3 #\.)))
  (pass? "palendrome :elide nil    3.8"  (string= (palindrome "ABC" :elide nil) "ABCCBA"))
  (pass? "palendrome :elide :last  3.9"  (string= (palindrome "ABC" :elide :last) "ABCBA"))
  (pass? "palendrome :elide :first 3.10" (string= (palindrome "ABC" :elide :first) "ABCCB"))
  (pass? "palendrome :elide :both  3.11"  (string= (palindrome "ABC" :elide :both) "ABCB")))
  
(multiple-value-bind (head tail)
    (parse-word " ape  bat cat")
  (pass? "parse-word head 3.12" (string= head "ape"))
  (pass? "parse-word tail 3.13" (string= tail "  bat cat")))

(let* ((test-string "This is a text")
       (words (split-string test-string)))
  (pass? "split-string 3.14"
	 (and (listp words)
	      (= (length words) 4)
	      (string= (first words) "This")
	      (string= (second words) "is")
	      (string= (third words) "a")
	      (string= (fourth words) "text")))
  (pass? "starts-with-p 3.15"
	 (and (starts-with-p test-string "This is")
	      (not (starts-with-p test-string " This")))))

(pass? "sformat 3.16" (string= (sformat "~A~A" 'A 'B) "AB"))
(pass? "format-binary 3.17" (string= (format-binary 9 :bits 4) "1001"))
(pass? "center-string 3.18" (string= "   APE   " (center-string "APE" 9)))

;;; string pick test
;;;
(let ((frequency-table (make-hash-table :size 3))
      (test-string "ABC")
      (test-runs 10000))
  (setf (gethash 'A frequency-table) 0
	(gethash 'B frequency-table) 0
	(gethash 'C frequency-table) 0)
  (dotimes (i test-runs)
    (let ((s (->symbol (sformat "~A" (pick test-string)))))
      (setf (gethash s frequency-table)
      	    (1+ (gethash s frequency-table)))))
  (let* ((A (float (/ (gethash 'A frequency-table) test-runs)))
	 (B (float (/ (gethash 'B frequency-table) test-runs)))
	 (C (float (/ (gethash 'C frequency-table) test-runs)))
	 (frequencies (list A B C)))
    (pass? "string pick 3.19"
	   (every #'(lambda (q)(and (>= q 0.25)(<= q 0.35))) frequencies))))


;;; string permute test
;;;
(let ((frequency-table (make-hash-table :size 6))
      (test-string "ABC")
      (test-runs 10000))
  (setf (gethash 'ABC frequency-table) 0
	(gethash 'ACB frequency-table) 0
	(gethash 'BAC frequency-table) 0
	(gethash 'BCA frequency-table) 0
	(gethash 'CAB frequency-table) 0
	(gethash 'CBA frequency-table) 0)
  (dotimes (i test-runs)
    (let ((s (->symbol (sformat "~A" (permute test-string)))))
      (setf (gethash s frequency-table)
      	    (1+ (gethash s frequency-table)))))
  (maphash #'(lambda (key frequency)
	       (let ((ratio (float (/ frequency test-runs))))
		 (pass? (sformat "string permute ~A  3.20" key) (and (>= ratio 0.1)(<= ratio 0.2)))))
	   frequency-table))
	



      
  
