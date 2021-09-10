;;;; CYCO Emu Procussion plugin: multi-fx
;;;;
;;;; Zone Stack             key range : 
;;;;  1 554 Rasparity      : 036 040  : 
;;;;  2 536 Qool Klanq     : 041      :
;;;;  3 308 FingerCymb     : 042      :
;;;;  4 440 Hallowell      : 043      :
;;;;  5 441 StarTree       : 044      :
;;;;  6 447 WarbleWave     : 045      :
;;;;  7 461 Ganga Log      : 046      :
;;;;  8 442 Thundadome     : 047 052  :
;;;;  9 525 Noise FX       : 053 055  :
;;;; 10 474 TinkerTine     : 065 071  :
;;;; 11 448 TempleBell     : 056 061  :
;;;; 12 554 Rasperity      : 062 064  :
;;;; 13 442 Thundadome     : 072 083  :
;;;; 14 475 Bellhause      : 084 096  :
;;;; 15                    :          : 
;;;; 16                    :          :
;;;; 17                    :          :
;;;; 18                    :          :
;;;; 19                    :          :
;;;; 20                    :          :
;;;; 21                    :          : 
;;;; 22                    :          :
;;;; 23                    :          : 
;;;; 24 311 SFX 1          : 098 098  :
;;;;

(let ((keylist '((bell           . (084))
		 (bell-2         . (085))
		 (bell-3         . (086))
		 (bell-4         . (087))
		 (bell-5         . (088))
		 (bell-6         . (089))
		 (bell-7         . (090))
		 (bell-8         . (091))
		 (bell-9         . (092))
		 (bell-10        . (093))
		 (bell-11        . (094))
		 (bell-12        . (095))
		 (bell-13        . (096))
		 (noise          . (053))
		 (noise-2        . (054))
		 (noise-3        . (055))
		 (rasp           . (036))
		 (rasp-2         . (037))
		 (rasp-3         . (038))
		 (rasp-4         . (039))
		 (rasp-5         . (040))
		 (rasp-6         . (040))
		 (rasp-7         . (062))
		 (rasp-8         . (063))
		 (rasp-9         . (064))
		 (temple         . (056))
		 (temple-2       . (057))
		 (temple-3       . (058))
		 (temple-4       . (059))
		 (temple-5       . (060))
		 (temple-6       . (061))
		 (thundadome     . (047))
		 (thundadome-2   . (048))
		 (thundadome-3   . (049))
		 (thundadome-4   . (050))
		 (thundadome-5   . (052))
		 (thundadome-6   . (072))
		 (thundadome-7   . (073))
		 (thundadome-8   . (074))
		 (thundadome-9   . (075))
		 (thundadome-10  . (076))
		 (thundadome-11  . (077))
		 (thundadome-12  . (078))
		 (thundadome-13  . (079))
		 (thundadome-14  . (080))
		 (thundadome-15  . (081))
		 (thundadome-16  . (083))
		 (tinker         . (065))
		 (tinker-2       . (066))
		 (tinker-3       . (067))
		 (tinker-4       . (068))
		 (tinker-5       . (069))
		 (tinker-6       . (070))
		 (tinker-7       . (071))
		 (warble         . (045))
		 (finger         . (042))
		 (log            . (046))
		 (hallowell      . (043))
		 (klang          . (041))
		 (sfx            . (098))
		 (startree       . (044)))))
  (defun multi-fx (&key (parent procussion) channel )
    (instrument multi-fx
		:parent parent
		:channel channel
		:program (procussion-program 'multi-fx)
		:keynumber-map (procussion-keymap 36 98 keylist)))) 

