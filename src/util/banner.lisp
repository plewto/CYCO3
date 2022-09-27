;;;; CYCO util banner.lisp
;;;;
;;;; Defines several text banners.
;;;;

(in-package :cyco)

(constant +BANNER-BAR1+ (scopies 68 #\*))
(constant +BANNER-HEADER1+ "*** ")
(constant +BANNER-BAR2+ (scopies 68 #\-))
(constant +BANNER-HEADER2+ "--- ")
(constant +BANNER-ERROR+ (sformat "ERROR ~A" (scopies 30 #\*)))
(constant +BANNER-WARNING+ (sformat "WARNING ~A" (scopies 30 #\*)))
(global *enable-banners* t)

(defun cyco-banner ()
  "Prints CYCO logo"
  (format t "~%~A~%" +BANNER+)
  (format t "Version ~A " +CYCO-VERSION+)
  (let ((year (cdr (assoc :year +build-time+)))
	(month (cdr (assoc :month +build-time+)))
	(day (cdr (assoc :day +build-time+)))
	(hour (cdr (assoc :hour +build-time+)))
	(minute (cdr (assoc :minute +build-time+))))
    (format t "Build: ~4D-~A-~A  ~A:~A~%" year month day hour minute)))

(flet ((banner-bar (q)
		   (format t "~A~%" q))
       (banner-headline (text header width)
			(format t "~A~A~%" header (center-string text width -6)))
       (banner-more (text-list header)
		    (if text-list
			(format t "~A~%" header))
		      (dolist (q text-list)
			(format t "~A~A~%" header q))))
  (defun banner1 (headline &rest more)
    (if *enable-banners*
	(progn 
	  "Prints text between banner bars."
	  (format t "~%")
	  (banner-bar +banner-bar1+)
	  (banner-headline headline +banner-header1+ 75)
	  (banner-more more +banner-header1+)
	  (banner-bar +banner-bar1+)
	  (format t "~%"))))

  (defun banner2 (headline &rest more)
    "Prints text between banner bars.
BANNER2 is less bold then BANNER1."
    (if *enable-banners*
	(progn 
	  (banner-bar +banner-bar2+)
	  (banner-headline headline +banner-header2+ 75)
	  (banner-more more +banner-header2+)
	  (format t "~%"))))

  (defun banner3 (headline &rest more)
    "Prints text between banner bars.
BANNER3 is less bold then BANNER2."
    (if *enable-banners*
	(progn 
	  (format t "~A~A~%" +banner-header2+ headline)
	  (dolist (q more)
	    (format t "~A   ~A~%" +banner-header2+ q))))) ) 
