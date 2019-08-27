;; test-5 grace notes

(section grace)

;; :grace
;; :grace-delay
;; :grace-amp*
;; :grace-duration

(format t "GRACE notes~%")

(strummer grace-1 piano1
	  :events '((:time 1.000 :key 60  :amp 0.75  :dur 1.000 :chord [maj])
		    (:grace 96 :grace-delay -1*t :grace-amp* 0.5 :grace-duration t)))

(dump-events grace-1)

