;;;; CYCO3 test/string-utilities
;;;;

(let* ((original "foo")
       (copy (clone original)))
  (pass? (eq original copy) "string clone"))


(let ((test-string "ABC"))
  (pass? (string= "C" (final test-string)) "string final")
  (pass? (string= "AB" (butfinal test-string)) "string butfinal")
  (pass? (and (string= "B" (cnth 1 test-string))(string= "B" (cnth 4 test-string))) "string cnth")
  (pass? (string= "ABCDEF" (str+ test-string "DEF")) "str+")
  (pass? (string= "  " (spaces 2)) "spaces")
  (pass? (string= "..." (scopies 3 #\.)) "scopies")
  (pass? (string= "ABCCBA" (palindrome test-string)) "palindrome (elide nil)")
  (pass? (string= "ABCBA" (palindrome test-string :elide :last)) "palindrome (elide :last)")
  (pass? (string= "ABCCB" (palindrome test-string :elide :first)) "palindrome (elide :first)")
  (pass? (string= "ABCB" (palindrome test-string :elide :both)) "palindrome (elide :both)")
  )


(multiple-value-bind (head tail)
    (parse-word "ape    bat cat")
  (pass? (and (string= head "ape")(string= tail "    bat cat")) "parse-word"))


(let* ((test-string "This is a text")
       (words (split-string test-string)))
  (pass? (and (string= (first words) "This")
	      (string= (second words) "is")
	      (string= (third words) "a")
	      (string= (fourth words) "text")
	      (= (length words) 4))
	 "split-string")

  (pass? (starts-with-p test-string "This is") "starts-with-p"))


(pass? (string= (sformat "~A~A" 'A 'B) "AB") "sformat")
(pass? (string= (format-binary 9 :bits 4) " 1001") "format-binary")
(pass? (string= "ape ball cat" (string-replace "bat" "ball" "ape bat cat")) "string-replace")
(pass? (string= "   APE   " (center-string "APE" 9)) "center-string")

;; Pick and permute demonstration (not test per se)

(let ((src "ABCD"))
  (dotimes (i 10)
    (format t "PICK ~A --> ~A~%" src (pick src)))
  (dotimes (i 10)
    (format t "PERMUTE ~A --> ~A~%" src (permute src))))
