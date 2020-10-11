;;;; cyco3 pigiron plugin util.lisp
;;;;



(defgeneric bool->string (arg))
(defgeneric string->bool (arg))


(labels ((is-false (arg)
		   (member (string-downcase arg)
			   (list "f" "false" "no" "off" "disable" "nil")
			   :test #'string=))
	 (is-true (arg)
		  (member (string-downcase arg)
			  (list "t" "true" "yes" "on" "enable")
			  :test #'string=))

	 (is-boolean (arg)
		     (or (is-false arg)
			 (is-true arg))))

  (defmethod bool->string ((arg t))
    (if arg "True" "False"))
  
  (defmethod bool->string ((arg string))
    (if (is-boolean arg)
	(if (is-true arg) "True" "False")
      (error (sformat "Expected boolean string, encounterd ~S" arg))))

  (defmethod bool->string ((s symbol))
    (bool->string (->string s)))

  (defmethod string->bool ((arg string))
    (if (is-true arg) t nil)))
	
      
  


