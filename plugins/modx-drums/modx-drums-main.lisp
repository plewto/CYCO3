;;;; CYCO modx-drums plugin
;;;; Defines drum-kit instruments for Yamaha MODX.

(plugin modx)
(in-package :modx)

(defparameter *kit-info* '())
(defparameter *loaded-kits* '())
(defparameter *current-drum-channel* 1)
(defparameter *current-drum-parent* *current-modx-performance*)

(defun register-kit (name &optional remarks)
  (push (cons name (format nil "~A" (or remarks ""))) *kit-info*)
  (setf *kit-info* (sort *kit-info* #'(lambda (a b)(string< (->string (car a))(->string (car b)))))))

(defun ?modx-drums (&optional (pattern ""))
  "Prints list of MODX drum kits."
  (format t ";; Available Yamaha MODX drum kits:~%")
  (dolist (item *kit-info*)
    (let* ((name (->string (car item)))
	   (pos (search (->string pattern) name)))
      (if pos
	  (format t ";;   ~16A ~A~%" (car item)(cdr item))))))

(defmacro drum-loader (kit-name &optional (remarks ""))
  `(progn
     (register-kit ',kit-name ,remarks)
     (defun ,kit-name (channel &key (performance *current-modx-performance*))
       (setf *current-drum-channel* channel)
       (setf *current-drum-parent* performance)
       (setf *loaded-kits* (adjoin ',kit-name *loaded-kits*))
       (external-load-plugin-file ',kit-name 'modx-drums))))

(defmacro make-main-instrument (name keylist &key (remarks ""))
  `(modx-instrument ,name *current-drum-channel*
		    :performance *current-drum-parent*
		    :keynumber-map (symbolic-keynumber-map ,keylist)
		    :remarks (->string ,remarks)))

(defmacro make-sub (name parent keylist &key (remarks ""))
  `(instrument ,name :parent ,parent
	       :keynumber-map (symbolic-keynumber-map ,keylist)
	       :remarks (->string ,remarks)))


(drum-loader arabic-mixed-kit-2)
(drum-loader brazil-kit)
(drum-loader dub-rock-4 "Instrument 4 for factory performance 'Dub Rock Bass'")
(drum-loader dub-rock-5 "Instrument 5 for factory performance 'Dub Rock Bass'")
(drum-loader iranian-mix-kit)
(drum-loader midnight-funk)
(drum-loader new-arabic-kit-1)
(drum-loader new-arabic-kit-2)
(drum-loader new-iranian-kit)
(drum-loader new-iranian-kit-2)
(drum-loader new-maple-custom-kit)
(drum-loader real-brushes-kit)
(drum-loader real-drums-kit)
(drum-loader schlager-weapon)
(drum-loader songwriter-2015 "Modification of Schlager-weapon")
(drum-loader t3-uber-funk)
(drum-loader turkish-kit)

(export '(?modx-drums
	  arabic-mixed-kit-2
	  brazil-kit
	  dub-rock-4
	  dub-rock-5
	  iranian-mix-kit
	  midnight-funk
	  new-arabic-kit-1
	  new-arabic-kit-2
	  new-iranian-kit
	  new-iranian-kit-2
	  new-maple-custom-kit
	  real-brushes-kit
	  real-drums-kit
	  schlager-weapon
	  songwriter-2015
	  t3-uber-funk
	  turkish-kit
	  ) :modx)

(import '(modx:?modx-drums
	  modx:arabic-mixed-kit-2
	  modx:brazil-kit
	  modx:dub-rock-4
	  modx:dub-rock-5
	  modx:iranian-mix-kit
	  modx:midnight-funk
	  modx:new-arabic-kit-1
	  modx:new-arabic-kit-2
	  modx:new-iranian-kit
	  modx:new-iranian-kit-2
	  modx:new-maple-custom-kit
	  modx:real-brushes-kit
	  modx:real-drums-kit
	  modx:schlager-weapon
	  modx:songwriter-2015
	  t3-uber-funk
	  turkish-kit
	  ) :cyco)
