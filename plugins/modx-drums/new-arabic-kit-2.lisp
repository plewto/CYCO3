;;;; CYCO modx-drums NEW-ARABIC-KIT-2
;;;;
;;;; 36 C1  3636 Bendir2 Lrg Dom Opn1
;;;; 37 C#1 3637 Bendir2 Lrg Dom Opn2
;;;; 38 D1  3638 Bendir2 Lrg Dom Cls
;;;; 39 D#1 3639 Bendir2 Lrg Sak1
;;;; 40 E1  3640 Bendir2 Lrg Sak2
;;;; 41 F1  3641 Bendir2 Lrg Tak Opn1
;;;; 42 F#1 3642 Bendir2 Lrg Tak Opn2
;;;; 43 G1  3646 Bendir2 Lrg Flam
;;;; 44 G#1 3643 Bendir2 Lrg 1
;;;; 45 A1  3644 Bendir2 Lrg 2
;;;; 46 A#1 3645 Bendir2 Lrg 3
;;;; 47 B1  3647 Bendir2 Lrg Flam&Tak
;;;; 48 C2  3365 Nakrazan Dom
;;;; 49 C#2 3366 Nakrazan Flam
;;;; 50 D2  3367 Nakrazan Tak 1
;;;; 51 D#2 3368 Nakrazan Tak 2
;;;; 52 E2  3369 Nakrazan Tak 3
;;;; 53 F2  3370 Nakrazan GlisDn pppp
;;;; 54 F#2 3371 Nakrazan GlisDn ppp
;;;; 55 G2  3372 Nakrazan GlisDn pp
;;;; 56 G#2 3373 Nakrazan GlisDn p
;;;; 57 A2  3374 Nakrazan Noise FX
;;;; 58 A#2 3375 Nakrazan Roll1
;;;; 59 B2  3376 Nakrazan Roll2
;;;; 60 C3  4058 Katem2 Dom1
;;;; 61 C#3 4059 Katem2 Dom2
;;;; 62 D3  4060 Katem2 Tak Open1
;;;; 63 D#3 4061 Katem2 Tak Open2
;;;; 64 E3  4064 Katem2 Sak
;;;; 65 F3  4065 Katem2 Sak Close1
;;;; 66 F#3 4066 Katem2 Sak Close2
;;;; 67 G3  4067 Katem2 Flam1
;;;; 68 G#3 4068 Katem2 Flam2
;;;; 69 A3  4062 Katem2 Tak Close1
;;;; 70 A#3 4063 Katem2 Tak Close2
;;;; 71 B3  4069 Katem2 Trem Dom
;;;; 72 C4  4070 Katem2 Med Dom Opn1
;;;; 73 C#4 4071 Katem2 Med Dom Opn2
;;;; 74 D4  4072 Katem2 Med Dom Opn3
;;;; 75 D#4 4073 Katem2 Med Dom Close
;;;; 76 E4  4074 Katem2 Med Tak1
;;;; 77 F4  4075 Katem2 Med Tak2
;;;; 78 F#4 4076 Katem2 Med Sak1
;;;; 79 G4  4077 Katem2 Med Sak2
;;;; 80 G#4 4080 Katem2 Med Flam
;;;; 81 A4  4078 Katem2 Med1
;;;; 82 A#4 4079 Katem2 Med2
;;;; 83 B4  4081 Katem2 Med Flam&Tak
;;;; 84 C5  4082 Katem2 Sml Dom Opn
;;;; 85 C#5 4083 Katem2 Sml Dom Cls1
;;;; 86 D5  4084 Katem2 Sml Dom Cls2
;;;; 87 D#5 4085 Katem2 Sml Tak1
;;;; 88 E5  4086 Katem2 Sml Tak2
;;;; 89 F5  4087 Katem2 Sml Sak
;;;; 90 F#5 4091 Katem2 Sml Flam
;;;; 91 G5  4088 Katem2 Sml1
;;;; 92 G#5 4089 Katem2 Sml2
;;;; 93 A5  4090 Katem2 Sml3
;;;; 94 A#5 4092 Katem2 Sml Flam&Tak
;;;; 95 B5  4093 Katem2 Sml Fing Ta

(in-package :modx)
(let* ((general-klist '((bendir-dom        . (36 "C1  3636 Bendir2 Lrg Dom Opn1"))
			(bendir-dom2       . (37 "C#1 3637 Bendir2 Lrg Dom Opn2"))
			(bendir-dom-closed . (38 "D1  3638 Bendir2 Lrg Dom Cls"))
			(bendir-sak        . (39 "D#1 3639 Bendir2 Lrg Sak1"))
			(bendir-sak2       . (40 "E1  3640 Bendir2 Lrg Sak2"))
			(bendir-tak        . (41 "F1  3641 Bendir2 Lrg Tak Opn1"))
			(bendir-tak2       . (42 "F#1 3642 Bendir2 Lrg Tak Opn2"))
			(bendir-flam       . (43 "G1  3646 Bendir2 Lrg Flam"))
			(bendir-A          . (44 "G#1 3643 Bendir2 Lrg 1"))
			(bendir-B          . (45 "A1  3644 Bendir2 Lrg 2"))
			(bendir-C          . (46 "A#1 3645 Bendir2 Lrg 3"))
			(bendir-flam-tak   . (47 "B1  3647 Bendir2 Lrg Flam&Tak"))
			
			(nakrazan-dom       . (48 "C2  3365 Nakrazan Dom"))
			(nakrazan-tak       . (50 "D2  3367 Nakrazan Tak 1"))
			(nakrazan-tak2      . (51 "D#2 3368 Nakrazan Tak 2"))
			(nakrazan-tak3      . (52 "E2  3369 Nakrazan Tak 3"))
			(nakrazan-flam      . (49 "C#2 3366 Nakrazan Flam"))
			(nakrazan-glis-pppp . (53 "F2  3370 Nakrazan GlisDn pppp"))
			(nakrazan-glis-ppp  . (54 "F#2 3371 Nakrazan GlisDn ppp"))
			(nakrazan-glis-pp   . (55 "G2  3372 Nakrazan GlisDn pp"))
			(nakrazan-glis-p    . (56 "G#2 3373 Nakrazan GlisDn p"))
			(nakrazan-noise     . (57 "A2  3374 Nakrazan Noise FX"))
			(nakrazan-roll      . (58 "A#2 3375 Nakrazan Roll1"))
			(nakrazan-roll2     . (59 "B2  3376 Nakrazan Roll2"))

			(katem-dom          . (60 "C3  4058 Katem2 Dom1"))
			(katem-dom2         . (61 "C#3 4059 Katem2 Dom2"))
			(katem-tak          . (62 "D3  4060 Katem2 Tak Open1"))
			(katem-tak2         . (63 "D#3 4061 Katem2 Tak Open2"))
			(katem-sak          . (64 "E3  4064 Katem2 Sak"))
			(katem-sak-closed   . (65 "F3  4065 Katem2 Sak Close1"))
			(katem-sak-closed2  . (66 "F#3 4066 Katem2 Sak Close2"))
			(katem-flam         . (67 "G3  4067 Katem2 Flam1"))
			(katem-flam2        . (68 "G#3 4068 Katem2 Flam2"))
			(katem-tak-closed   . (69 "A3  4062 Katem2 Tak Close1"))
			(katem-tak-closed2  . (70 "A#3 4063 Katem2 Tak Close2"))
			(katem-tremolo      . (71 "B3  4069 Katem2 Trem Dom"))
			
			(katem2-dom         . (72 "C4  4070 Katem2 Med Dom Opn1"))
			(katem2-dom2        . (73 "C#4 4071 Katem2 Med Dom Opn2"))
			(katem2-dom3        . (74 "D4  4072 Katem2 Med Dom Opn3"))
			(katem2-dom-closed  . (75 "D#4 4073 Katem2 Med Dom Close"))
			(katem2-tak         . (76 "E4  4074 Katem2 Med Tak1"))
			(katem2-tak2        . (77 "F4  4075 Katem2 Med Tak2"))
			(katem2-sak         . (78 "F#4 4076 Katem2 Med Sak1"))
			(katem2-sak2        . (79 "G4  4077 Katem2 Med Sak2"))
			(katem2-flam        . (80 "G#4 4080 Katem2 Med Flam"))
			(katem2-A           . (81 "A4  4078 Katem2 Med1"))
			(katem2-B           . (82 "A#4 4079 Katem2 Med2"))
			(katem2-flam-tak    . (83 "B4  4081 Katem2 Med Flam&Tak"))
			
			(katem3-dom-open     . (84 "C5  4082 Katem2 Sml Dom Opn"))
			(katem3-dom-closed   . (85 "C#5 4083 Katem2 Sml Dom Cls1"))
			(katem3-dom-closed2  . (86 "D5  4084 Katem2 Sml Dom Cls2"))
			(katem3-tak          . (87 "D#5 4085 Katem2 Sml Tak1"))
			(katem3-tak2         . (88 "E5  4086 Katem2 Sml Tak2"))
			(katem3-sak          . (89 "F5  4087 Katem2 Sml Sak"))
			(katem3-flam         . (90 "F#5 4091 Katem2 Sml Flam"))
			(katem3-A            . (91 "G5  4088 Katem2 Sml1"))
			(katem3-B            . (92 "G#5 4089 Katem2 Sml2"))
			(katem3-C            . (93 "A5  4090 Katem2 Sml3"))
			(katem3-tak3         . (95 "B5  4093 Katem2 Sml Fing Ta"))
			(katem3-flam-tak     . (94 "A#5 4092 Katem2 Sml Flam&Tak"))))
       (bendir-klist (extract-sub-symbolic-keylist 'bendir general-klist))
       (nakrazan-klist (extract-sub-symbolic-keylist 'nakrazan general-klist))
       (katem-klist (extract-sub-symbolic-keylist 'katem general-klist)) 
       (katem2-klist (extract-sub-symbolic-keylist 'katem2 general-klist)) 
       (katem3-klist (extract-sub-symbolic-keylist 'katem3 general-klist)) 
       (nak2 (make-main-instrument new-arabic-kit-2 general-klist :remarks "Small set of frame drums")) )
  (make-sub nak2-bendir nak2 bendir-klist)
  (make-sub nak2-nakrazan nak2 nakrazan-klist)
  (make-sub nak2-katem nak2 katem-klist :remarks "Large katem frame drum")
  (make-sub nak2-katem2 nak2 katem2-klist :remarks "Medium katem frame drum")
  (make-sub nak2-katem3 nak2 katem3-klist :remarks "Small katem frame drum"))

(export '(nak2-bendir
	  nak2-nakrazan
	  nak2-katem
	  nak2-katem2
	  nak2-katem3) :modx)

(import '(modx:nak2-bendir
	  modx:nak2-nakrazan
	  modx:nak2-katem
	  modx:nak2-katem2
	  modx:nak2-katem3) :cyco)
