;;;; MODX drums songwriter-2015
;;;; A modified version of Schlager-weapon
;;;; Renames schlager-weapon to songwriter-2015
;;;;

(in-package :modx)

(external-load-plugin-file 'schlager-weapon 'modx-drums)

(multiple-value-bind (junk schlager-klist)
		      (funcall (keynumber-map schlager-weapon) :assignments)
		     (declare (ignore junk))
		     (let* ((general-klist '()))
		       (dolist (element schlager-klist)
			 (cond ((eq (car element) 'tambourine)
				(push '(HIQ-E . (62 "2276 Mini Brip1")) general-klist))
			       ((eq (car element) 'shaker)
				(push '(FXPERC-F. (70 "2509 Electric Perc13")) general-klist))
			       (t (push element general-klist))))
		       (setf general-klist (reverse general-klist))
		     (set-name schlager-weapon 'songwriter-2015)
		     (defparameter songwriter-2015 schlager-weapon)
		     (set-keynumber-map sw-shaker (symbolic-keynumber-map
						   (extract-sub-symbolic-keylist 'shaker general-klist)))
		     (set-keynumber-map sw-hiq (symbolic-keynumber-map
		     				(extract-sub-symbolic-keylist 'hiq general-klist)))
		     (set-keynumber-map sw-fxperc (symbolic-keynumber-map
						   (extract-sub-symbolic-keylist 'fxperc general-klist))) ))
					      
		
