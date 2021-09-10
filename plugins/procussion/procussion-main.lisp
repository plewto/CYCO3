;;;; CYCO Emu Procussion plugin 
;;;;
;;;; The Emu Procussion is essentially a Proteus dedicated to percussion.
;;;; It has 128 memory slots, the first 64 are factory ROM, the final 64
;;;; are user RAM.  Procussion programs are multi timbrel with up to 24
;;;; "zones" per kit.  Zones operate over specific key ranges which may
;;;; overlap. Several kits are designated as "standard". All such kits
;;;; are composed of a standard set of instruments with specific key
;;;; ranges.
;;;;
;;;; Non-standard kits have a wide variety of sub-instruments at various
;;;; key positions.  Finally there are a few tuned instruments such as
;;;; vibraphone.  The standard and tuned instruments are the easiest to
;;;; deal with.  The other kits are treated more or less individually.
;;;;
;;;; Each kit defines an instrument with the kit's name and usually several
;;;; sub-instruments with varius keynumber maps. 
;;;; 

(global *PROCUSSION-PROGRAMS*
	  '((ampitheater     . (000 standard)) 
	    (mega-drums      . (001 standard)) 
	    (rock-n-roll     . (002 standard)) 
	    (palladium       . (003 standard)) 
	    (jazz-drums      . (004 standard)) 
	    (metal-drums     . (005 standard)) 
	    (rap-session     . (006 standard)) 
	    (latin-drums     . (007 ))         
	    (percussion1     . (008 ))                 
	    (toolkit         . (009 ))         
	    (ambient-rock    . (010 standard)) 
	    (acoustic-kit    . (011 standard)) 
	    (thundadome      . (012 tuned2))     
	    (disco-ball      . (013 ))         
	    (vibrations      . (014 tuned))    
	    (rock-drums      . (015 standard)) 
	    (house-machine   . (016 standard)) 
	    (fusion-stix     . (017 standard)) 
	    (found-sound     . (018 ))         
	    (marimba         . (019 tuned))    
	    (space-drums     . (020 standard)) 
	    (hard-rock       . (021 standard)) 
	    (stadium-rox     . (022 standard)) 
	    (dance-2000      . (023 standard)) 
	    (industry        . (024 tuned2))   
	    (heavy-metal     . (025 standard)) 
	    (hip-hop         . (026 standard)) 
	    (rosewood        . (027 tuned))    
	    (percussion2     . (028 ))         
	    (sound-fx        . (029 ))         
	    (sluggo-drums    . (030 standard)) 
	    (beatbox         . (031 ))         
	    (rocket-drums    . (032 ))         
	    (huge-room       . (033 standard)) 
	    (churchyard      . (034 tuned2))   
	    (drum-dance      . (035 standard)) 
	    (percussion3     . (036 ))         
	    (malletbells     . (037 tuned))    
	    (hip-house       . (038 ))         
	    (latin-layers    . (039 ))         
	    (big-band        . (040 ))         
	    (multi-fx        . (041 ))         
	    (heavyosity      . (042 standard)) 
	    (ritual-night    . (043 tuned2))   
	    (dance-club      . (044 standard)) 
	    (indo-steel      . (045 tuned))    
	    (orch-percussion . (046))          
	    (country-kit     . (047 standard)) 
	    (killer-synth    . (048 tuned))    
	    (heavy-handed    . (049 ))         
	    (percussives     . (050 ))         
	    (mystic-land     . (051 tuned))    
	    (beach-party     . (052 ))         
	    (intervallix     . (053 tuned2))   
	    (jazzy-traps     . (054 ))         
	    (clavarimba      . (055 tuned))    
	    (rockabilly      . (056 standard)) 
	    (control-snares  . (057 ))         
	    (lotsa-kicks     . (058 ))  ;** NOT DEFINED
	    (all-snares      . (059 ))  ;** NOT DEFINED
	    (more-toms       . (060 ))  ;** NOT DEFINED
	    (more-cymbals    . (061 ))  
	    (more-percussion . (062 )) 
	    (more-basses     . (063 tuned3)))) 


(instrument procussion
	    :parent +root-instrument+
	    :remarks "Default root for Emu Procussion instruments."
	    :transient nil)

(defun add-procussion-kit (name program-number &optional remarks)
  "Assigns program number for user kit."
  (setf *procussion-programs* (append *procussion-programs*
				      (list (cons name (list program-number (or remarks "")))))))
	


(set-symbolic-program-map procussion *procussion-programs* :offset 0)

(defun procussion-program (prg)
  "Returns program number for Emu Procussion kit."
  (or (and (integerp prg)(<= 0 prg)(< prg 128) prg)
      (second (assoc prg *procussion-programs*))
      (progn 
	(cyco-warning
	 (sformat "Invalid Emu Procussion program: ~A" prg)
	 "See *PROCUSSION-PROGRAMS*"
	 "Using default 0")
	0)))

(defun procussion-keymap (min max &optional alist)
  "Returns keymap for top-level Procussion instruments.
The keymap may take standard keynumbers over between min and max,
namd may have an optional symbolic keymap."
  #'(lambda (n)
      (cond ((eq n :doc)
	     (format t "Keynumber range ~D..~D~%" min max)
	     (if alist
		 (progn 
		   (format t "x --> default ~A~%" (car (car alist))) 
		   (dolist (item alist)
		     (format t "~8A --> ~A~%" (car item)(cdr item)))))
	     +rest+)
	    ((rest-p n) +rest+)
	    ((keynumber-p n)
	     (let ((kn (keynumber n)))
	       (or (and (>= kn min)(<= kn max) kn)
		   +rest+)))
	    (t (or (second (assoc n alist))
		   +rest+)))))

(defun procussion-subkey-map (alist)
  "Returns keynumber map for Provussion sub-instruments.
The keymap behaves like a combination of a circular-keymap and 
a symbolic-keymap." 
  #'(lambda (n)
      (cond ((eq n :doc)
	     (format t "Keynumber circular range 0..~D~%" (1- (length alist)))
	     (format t "x --> default ~A~%" (car (car alist)))
	     (dolist (item alist)
	       (format t "~8A -> ~A~%" (car item)(cdr item)))
	     +rest+)
	    ((rest-p n) +rest+)
	    ((integerp n)
	     (second (cnth n alist)))
	    ((keynumber-p n)
	     (second (cnth (keynumber n) alist)))
	    ((eq n 'x)
	     (second (car alist)))
	    (t (or (second (assoc n alist))
		   +rest+)))))

(defun ?procussion ()
  (format t "Emu Procussion programs~%")
  (dolist (item *procussion-programs*)
    (if (= (second item) 64)
	(format t "    ---- USER ----~%"))
    (format t "    ~18A  program ~3D  ~A~%" (car item)(second item)(or (third item) ""))))

(load-plugin-file "standard-kit")
(load-plugin-file "tuned-instruments")
(load-plugin-file "beach-party")
(load-plugin-file "beatbox")
(load-plugin-file "big-band")
(load-plugin-file "control-snares")
(load-plugin-file "disco-ball")
(load-plugin-file "found-sound")
(load-plugin-file "heavy-handed")
(load-plugin-file "hip-house")
(load-plugin-file "jazzy-traps")
(load-plugin-file "latin-drums")
(load-plugin-file "latin-layers")
(load-plugin-file "more-cymbals")
(load-plugin-file "multi-fx")
(load-plugin-file "orch-percussion")
(load-plugin-file "percussion-1")
(load-plugin-file "percussion-2")
(load-plugin-file "percussion-3")
(load-plugin-file "percussives")
(load-plugin-file "rocket-drums")
(load-plugin-file "sound-fx")
(load-plugin-file "toolkit")
