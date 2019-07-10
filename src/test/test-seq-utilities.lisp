;;;; CYCO3 test-seq-utilities
;;;;

(let ((test-list '(A B C))
      (test-vector #(X Y Z)))
  (pass? (same-thing-p (->list 'APE) '(APE)) "->list (t)")
  (pass? (eq (->list test-list) test-list) "->list (list)")
  (pass? (same-thing-p (->list test-vector) '(X Y Z)) "->list (vector)")
  (pass? (same-thing-p (->vector 'ape) #(ape)) "->vector (t)")
  (pass? (same-thing-p (->vector test-list) #(A B C)) "->vector (list)")
  (pass? (eq (->vector test-vector) test-vector) "->vector (vector)")
  (pass? (eq (final test-list) 'C) "final (list)")
  (pass? (eq (final test-vector) 'Z) "final (vector)")
  (pass? (same-thing-p (butfinal test-list) '(A B)) "butfinal (list)")
  (pass? (same-thing-p (butfinal test-vector) #(X Y)) "butfinal (vector)")
  (pass? (and (eq (cnth 1 test-list) 'B)
	      (eq (cnth 4 test-list) 'B))
	 "cnth (list)")
  (pass? (and (eq (cnth 1 test-vector) 'Y)
	      (eq (cnth 4 test-vector) 'Y))
	 "cnth (vector)")
  (pass? (same-thing-p (slice test-list 0) test-list) "slice  (0 nil)")
  (pass? (same-thing-p (slice test-list 1) '(B C)) "slice  (1 nil)")
  (pass? (same-thing-p (slice test-list 0 -1) '(A B)) "slice  (0 -1)")
  (pass? (same-thing-p (flatten1 '(A B (C D))) '(A B C D)) "flatten1")
  (pass? (same-thing-p (flatten '(A B (C (D)))) '(A B C D)) "flatten")
  (pass? (same-thing-p (rotate test-list 1) '(B C A)) "rotate 1")
  (pass? (same-thing-p (rotate test-list -1) '(C A B)) "rotate -1")
  (pass? (same-thing-p (zip '(A B C) '(E F G)) '(A E B F C G)) "zip")
  (pass? (same-thing-p (palindrome test-list) '(A B C C B A)) "palindrome (list)")
  (pass? (same-thing-p (palindrome test-vector) #(X Y Z Z Y X)) "palindrome (vector)")
  (pass? (= 55 (apply #'+ (range 0 11))) "range")
  
  (pass? (same-thing-p (fill-list '(A nil C) '(W X Y Z)) '(A X C Z)) "fill-list"
	 "Fails due to an extra NIL element at end of resulting list.")
  
  (multiple-value-bind (head tail)(split-list '(:ape 1 2 :bat 3 4 :cat 5 6))
    (pass? (and (same-thing-p head '(:ape 1 2))
		(same-thing-p tail '(:bat 3 4 :cat 5 6)))
	   "split-list"))

  ;; pick and permute demonstration (not tested per se)
  (dotimes (i 10)
    (format t "PICK list ~A --> ~A~%" test-list (pick test-list)))
  (dotimes (i 10)
    (format t "PERMUTE list ~A --> ~A~%" test-list (permute test-list)))
  (dotimes (i 10)
    (format t "PICK vector ~A --> ~A~%" test-vector (pick test-vector)))
  (dotimes (i 10)
    (format t "PERMUTE vector ~A --> ~A~%" test-vector (permute test-vector))))


(not-tested "sort-midi-events")


