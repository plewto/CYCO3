;;;; CYCO MODX-DRUMKITS
;;;;
;;;; Defines standard drum kits for Yamaha MODX.
;;;;

(in-package :cyco)
(plugin modx)
(plugin general-midi)

(let ((info '()))

  (defun register-modx-drumkit-info (name &optional remarks)
    (push (cons name (->string (or remarks ""))) info))
  
  (defun ?modx-drumkits ()
    (format t ";; Available Yamaha MODX drumkits: ~%")
    (dolist (k (sort (clone info) #'(lambda (a b)(string< (->string (car a))(->string (car b))))))
      (format t ";;    ~16A " (car k))
      (and (cdr k)(format t "~A" (cdr k)))
      (format t "~%"))))


(load-plugin-file "arabic-mixed-kit")
(load-plugin-file "iranian-mix-kit")
(load-plugin-file "new-maple-custom-kit")
(load-plugin-file "midnight-funk")
(load-plugin-file "real-brushes-kit")
(load-plugin-file "real-drums-kit")
(load-plugin-file "schlager-weapon")

