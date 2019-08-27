;;;; test 03-string-utilities
;;;;

(let* ((original "foo")
       (copy (clone original)))
  (pass? "string clone" (eq original copy)))

(let ((test-string "ABC"))
  (pass? "final" (string= (final test-string) "C"))
  (pass? "butfinal" (string= (butfinal test-string) "AB"))
  (pass? "string cnth" (and (string= "B" (cnth 1 test-string))
			    (string= "B" (cnth 4 test-string))))
  (pass? "str+" (string= (str+ "AB" "CD") "ABCD"))
  (pass? "spaces" (string= "    " (spaces 4)))
  (pass? "scopies" (string= "..." (scopies 3 #\.)))
  (pass? "palendrome :elide nil  "  (string= (palindrome "ABC" :elide nil) "ABCCBA"))
  (pass? "palendrome :elide :last"  (string= (palindrome "ABC" :elide :last) "ABCBA"))
  (pass? "palendrome :elide :first" (string= (palindrome "ABC" :elide :first) "ABCCB"))
  (pass? "palendrome :elide :both"  (string= (palindrome "ABC" :elide :both) "ABCB")))
  
(multiple-value-bind (head tail)
    (parse-word " ape  bat cat")
  (pass? "parse-word head" (string= head "ape"))
  (pass? "parse-word tail" (string= tail "  bat cat")))

(let* ((test-string "This is a text")
       (words (split-string test-string)))
  (pass? "split-string"
	 (and (listp words)
	      (= (length words) 4)
	      (string= (first words) "This")
	      (string= (second words) "is")
	      (string= (third words) "a")
	      (string= (fourth words) "text")))
  (pass? "starts-with-p"
	 (and (starts-with-p test-string "This is")
	      (not (starts-with-p test-string " This")))))

(pass? "sformat" (string= (sformat "~A~A" 'A 'B) "AB"))
(pass? "format-binary" (string= (format-binary 9 :bits 4) " 1001"))
(pass? "string-replace" (string= "ape bell cat" (string-replace "bat" "bell" "ape bat cat")))
(pass? "center-string" (string= "   APE   " (center-string "APE" 9)))

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
    (pass? "string pick"
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
		 (pass? (sformat "string permute ~A" key) (and (>= ratio 0.1)(<= ratio 0.2)))))
	   frequency-table))
	



      
  
