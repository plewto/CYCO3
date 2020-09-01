;;;; test 10-nodes
;;;;

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

  (pass? "name" (eq (name animal) 'animal))
  (pass? "remarks" (string= (remarks animal) "Animal is the root test node."))
  (pass? "root-p"
	 (and (root-p animal)
	      (not (root-p bird))))
  (pass? "child-of-p"
	 (and (child-of-p bird crow)
	      (not (child-of-p bird shark))))
  (pass? "find-child"
	 (and (eq (find-child bird crow) crow)
	      (eq (find-child fish 'trout) trout)
	      (not (find-child bird fish))
	      (not (find-child bird 'fish))))
  (pass? "path-to-root"
	 (equal (path-to-root shark) (list shark fish animal)))


  (progn
    (disconnect crow)
    (pass? "disconnect" (not (child-of-p bird crow)))
    (connect bird crow)
    (pass? "connect" (child-of-p bird crow)))

  (let ((alice (make-instance 'cyco-node :name 'alice :transient t))
	(bob (make-instance 'cyco-node :name 'bob :transient t)))
    (connect crow alice)
    (connect trout bob)
    (prune animal)
    (pass? "prune"
	   (and (root-p alice)
		(root-p bob)
		(not (root-p crow)))))

  (prune animal :force)
  (pass? "force prune"
	 (every #'root-p (list animal bird crow hawk fish trout shark)))

  ;; rebuild tree
  (connect animal bird)
  (connect bird crow)
  (connect bird hawk)
  (connect animal fish)
  (connect fish trout)
  (connect fish shark)
  
  (pass? "has-property-p"
	 (and (has-property-p animal 'diet)
	      (not (has-property-p bird 'foo))
	      (not (has-property-p nil nil))))
	      

  (pass? "property"
	 (and (eq (property animal 'diet) 'food)
  	      (eq (property bird 'diet) 'food)    ;; inherited
  	      (eq (property crow 'diet) 'worms))) ;; shadow

  (not-tested 'local-properties)
  (not-tested "(CLONE node)")
  (not-tested "(->STRING node)")
  )
