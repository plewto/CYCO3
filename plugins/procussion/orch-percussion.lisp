;;;; CYCO Emu Procussion plugin: orch-percussion
;;;;
;;;; Zone Stack             key range : 
;;;;  1 071 Timpani        : 053 072  : 
;;;;  2 067 Orch Bass      : 036      :
;;;;  3 080 BrushSnr A     : 040      :
;;;;  4 081 BrushSnr B     : 041      :
;;;;  5 082 BrushSnr C     : 043      :
;;;;  6 090 Room Snare 1   : 038      :
;;;;  7 100 Tonal Snare    : 039      :
;;;;  8 103 Tite Snare     : 047      :
;;;;  9 091 Room Snare 2   : 045      :
;;;; 10                    :          :
;;;; 11                    :          :
;;;; 12 073 KettleDrm2     : 048 052  :
;;;; 13                    :          :
;;;; 14                    :          :
;;;; 15                    :          : 
;;;; 16                    :          :
;;;; 17                    :          :
;;;; 18                    :          :
;;;; 19 293 China Gong     : 042      :
;;;; 20 368 Tambourine     : 037      :
;;;; 21 308 FingerCym      : 084 096  : 
;;;; 22 292 MallCymbal     : 046      :
;;;; 23 294 MalletRoll     : 044      : 
;;;; 24 362 Triangle       : 073 084  :
;;;;

(let ((keylist '((bass          . (36))
	       (tambourine    . (37))
	       (timpani-F     . (53))
	       (timpani-FS    . (54))
	       (timpani-G     . (55))
	       (timpani-GS    . (56))
	       (timpani-A     . (57))
	       (timpani-AS    . (58))
	       (timpani-B     . (59))
	       (timpani-C1    . (60))
	       (timpani-CS1   . (61))
	       (timpani-D1    . (62))
	       (timpani-DS1   . (63))
	       (timpani-E1    . (64))
	       (timpani-F2    . (65))
	       (timpani-FS2   . (66))
	       (timpani-G2    . (67))
	       (timpani-GS2   . (68))
	       (timpani-A2    . (69))
	       (timpani-AS2   . (70))
	       (timpani-B2    . (71))
	       (timpani-C2    . (72))
	       (kettle-C      . (48))
	       (kettle-CS     . (49))
	       (kettle-D      . (50))
	       (kettle-DS     . (51))
	       (kettle-E      . (52))
	       (fingercym-C   . (84))
	       (fingercym-CS  . (85))
	       (fingercym-D   . (86))
	       (fingercym-DS  . (87))
	       (fingercym-E   . (88))
	       (fingercym-F   . (89))
	       (fingercym-FS  . (90))
	       (fingercym-G   . (91))
	       (fingercym-GS  . (92))
	       (fingercym-A   . (93))
	       (fingercym-AS  . (94))
	       (fingercym-B   . (95))
	       (fingercym-C1  . (96))
	       (triangle-CS   . (73))
	       (triangle-D    . (74))
	       (triangle-DS   . (75))
	       (triangle-E    . (76))
	       (triangle-F    . (77))
	       (triangle-FS   . (78))
	       (triangle-G    . (79))
	       (triangle-GS   . (80))
	       (triangle-A    . (81))
	       (triangle-AS   . (82))
	       (triangle-B    . (83))
	       (triangle-C    . (84))
	       (gong          . (42))
	       (cymbal        . (46))
	       (cymbal-roll   . (44))
	       (brush-A . (40))
	       (brush-B . (41))
	       (brush-C . (43))
	       (snare-A . (38 "Room Snare 1"))
	       (snare-B . (39 "Tonal Sanre"))
	       (snare-C . (47 "Tite Snare"))
	       (snare-D . (45 "Room Sanre 2")))))
  (defun orch-percussion (&key (parent procussion) channel)
    (instrument orch-percussion
		:parent parent
		:channel channel
		:keynumber-map (procussion-keymap 36 96 keylist))))


