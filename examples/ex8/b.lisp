;;;; ex8 BINCUE the easy way.
;;;;

(section b :bars 8 :beats 4)

;; The BINBALL and BINXBALL macros combine a BINBALL with a QBALL or XBALL
;; parts respectively.   The usage BINBALL and BINXBALL are almost exactly
;; identical to the QBALL and XBALL macros with the following differences.
;;
;;  1) There is no :CUEFN argument, they both use the BAR function.
;;  2) There is a new :USE-SUBBEATS argument.  See section c.
;;  3) There is a new :SYMBOLS argument used to define user BINCUE symbols.
;;  4) There is a new :PRINT-CUE-LIST argument. If true the translated
;;     cue-list is printed.
;;
;;  The new objects appear in the project tree as QBALL or XQBALL parts.
;;

(BINBALL b-piano piano
	 :bars 2
	 :symbols '((clave-1 . "1000 0010 0010 0000")
		    (clave-2 . "0000 1000 1000 0000"))
	 :cue '(clave-1 clave-2)
	 :key '60
	 :dur 'e
	 :amp 'ff)


		    
