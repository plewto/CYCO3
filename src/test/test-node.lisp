
(let* ((properties '(diet environment limbs sound))
       (animal (make-instance 'cyco-node
			       :name 'animal
			       :remarks "Animal is the root test node."
			       :transient nil
			       :properties properties))
       (bird (make-instance 'cyco-node
			     :name 'bird
			     :transient nil
			     :properties properties))
       (crow (make-instance 'cyco-node
			     :name 'crow
			     :transient nil
			     :properties properties))
       (hawk (make-instance 'cyco-node
			     :name 'hawk
			     :transient nil
			     :properties properties))
       (fish (make-instance 'cyco-node
			    :name 'fish
			    :transient nil
			    :properties properties))
       (trout (make-instance 'cyco-node
			     :name 'trout
			     :transient nil
			     :properties properties))
       (shark (make-instance 'cyco-node
			     :name 'shark
			     :transient nil
			     :properties properties)))

  (connect animal bird)
  (connect bird crow)
  (connect bird hawk)
  (connect animal fish)
  (connect fish trout)
  (connect fish shark)
  (print-tree animal)

  (put animal 'diet 'food)
  (put animal 'environment 'earth)

  (put bird 'environment 'sky)
  (put bird 'limbs 'wing)
  (put bird 'sound 'screach)
  (put crow 'diet 'worms)
  (put hawk 'diet 'pidgeon)

  (put fish 'environment 'water)
  (put fish 'limbs 'fins)
  (put fish 'sound 'none)
  (put trout 'diet 'hooks)
  (put shark 'diet 'trout)
  
  (pass? (eq (name animal) 'animal) "name")
  (pass? (string= (remarks animal) "Animal is the root test node.") "remarks")
  (pass? (and (root-p animal)
	      (not (root-p bird))) "root-p")
  (pass? (and (child-of-p bird crow)
	      (not (child-of-p bird shark)))
	 "child-of-p")

  (pass? (and (eq (find-child bird crow) crow)
	      (eq (find-child fish 'trout) trout)
	      (not (find-child bird fish))
	      (not (find-child bird 'fish)))
	 "find-child")
  
  (pass? (same-thing-p (path-to-root shark)(list shark fish animal))
	 "path-to-root")

  (progn
    (disconnect crow)
    (pass? (not (child-of-p bird crow)) "disconnect")
    (connect bird crow)
    (pass? (child-of-p bird crow) "connect"))

  (let ((alice (make-instance 'cyco-node :name 'alice :transient t))
	(bob (make-instance 'cyco-node :name 'bob :transient t)))
    (connect crow alice)
    (connect trout bob)
    (prune animal)
    (pass? (and (root-p alice)
		(root-p bob)
		(not (root-p crow)))
	   "prune (:force nil)"))

  (prune animal :force)
  (pass? (every #'root-p (list animal bird crow hawk fish trout shark)) "prune (:force t)")

  ;; rebuid tree
  (connect animal bird)
  (connect bird crow)
  (connect bird hawk)
  (connect animal fish)
  (connect fish trout)
  (connect fish shark)

  (pass? (and (has-property-p bird 'diet)
	      (not (has-property-p bird 'foo))
	      (not (has-property-p nil nil)))
	 "has-property-p")

  (pass? (and (eq (property animal 'diet) 'food)
	      (eq (property bird 'diet) 'food)
	      (eq (property crow 'diet) 'worms))
	 "property")

  (fail "(properties node)" "Repeated property keys.")
  (format t "(properties crow)       --> ~A~%~%" (properties crow))

  (fail "(local-properties node)" "Weird list format.")
  (format t "*** Would changing format have 'down-stream' consequences ?~%")
  (format t "(local-properties crow) --> ~A~%~%" (local-properties crow)))

(not-tested "(clone cyco-node)")
(not-tested "(->string cyco-node)")

  
  
