;;;; CYCO modx-drums NEW-ARABIC-KIT-1
;;;;
;;;; 25 C#0 3720 Sagat2 Close
;;;; 26 D0  3722 Sagat3 Close
;;;; 27 D#0 3721 Sagat3 Open
;;;; 28 E0  3724 Sagat4 Close
;;;; 29 F0  3723 Sagat4 Open
;;;; 30 F#0 3826 Mazhar Dom Open
;;;; 31 G0  3827 Mazhar Dom Close
;;;; 32 G#0 3829 Mazhar Tak Close
;;;; 33 A0  3828 Mazhar Tak Open
;;;; 34 A#0 3830 Mazhar Sak Close
;;;; 35 B0  4052 Katem1 Dom1
;;;; 36 C1  4054 Katem1 Tak Open1
;;;; 37 C#1 4053 Katem1 Dom2
;;;; 38 D1  4055 Katem1 Tak Open2
;;;; 39 D#1 4056 Katem1 Sak1
;;;; 40 E1  3337 Tabel Saaidi Dom
;;;; 41 F1  3284 Djembe4 Tak1
;;;; 42 F#1 3338 Tabel Saaidi Tak
;;;; 43 G1  3285 Djembe4 Tak2
;;;; 44 G#1 3340 Tabel Dom
;;;; 45 A1  3341 Tabel Tak
;;;; 46 A#1 3342 Tabel Tak Wood
;;;; 47 B1  3770 Riq3 Dom Close
;;;; 48 C2  3769 Riq3 Dom Open
;;;; 49 C#2 3772 Riq3 Tak Close1
;;;; 50 D2  3771 Riq3 Tak Open
;;;; 51 D#2 3773 Riq3 Tak Close2
;;;; 52 E2  3774 Riq3 Tak Snouj
;;;; 53 F2  3768 Riq2 Sak
;;;; 54 F#2 3775 Riq3 Snouj Close1
;;;; 55 G2  3776 Riq3 Snouj Close2
;;;; 56 G#2 3778 Riq3 Flam
;;;; 57 A2  3777 Riq3 Sak
;;;; 58 A#2 3779 Riq3 Snouj Roll
;;;; 59 B2  3963 Darbuka2 Dom LH
;;;; 60 C3  3964 Darbuka2 Dom RH
;;;; 61 C#3 3966 Darbuka2 TakLIndxFng
;;;; 62 D3  3967 Darbuka2 TakRIndxFng
;;;; 63 D#3 3965 Darbuka2 Sak1
;;;; 64 E3  3968 Darbuka2 TakLRingFng
;;;; 65 F3  3970 Darbuka2 Tap1
;;;; 66 F#3 3971 Darbuka2 Tap2
;;;; 67 G3  3972 Darbuka2 Flam1
;;;; 68 G#3 3974 Darbuka2 Tak Roll
;;;; 69 A3  4177 Arabic Zalgouta Opn
;;;; 70 A#3 4178 Arabic Zalgouta Cls
;;;; 71 B3  4024 Daholla1 Dom LH
;;;; 72 C4  4025 Daholla1 Dom RH
;;;; 73 C#4 4026 Daholla1 Tak1
;;;; 74 D4  4027 Daholla1 Tak2
;;;; 75 D#4 4028 Daholla1 Tak3
;;;; 76 E4  4029 Daholla1 Tak4
;;;; 77 F4  4030 Daholla1 Sak
;;;; 78 F#4 4031 Daholla1 Rak1
;;;; 79 G4  2897 Arabic Hand Clap
;;;; 80 G#4 4033 Daholla1 Flam1
;;;; 81 A4  4034 Daholla1 Flam2
;;;; 82 A#4 4035 Daholla1 Tak Roll
;;;; 83 B4  3292 Tabla2 Dom & Tak
;;;; 84 C5  3293 Tabla2 Dom
;;;; 85 C#5 3294 Tabla2 Tak1
;;;; 86 D5  3295 Tabla2 Tak2
;;;; 87 D#5 3296 Tabla2 Tak3
;;;; 88 E5  3297 Tabla2 Tak4
;;;; 89 F5  3298 Tabla2 Sak
;;;; 90 F#5 3299 Tabla2 Rak1
;;;; 91 G5  3300 Tabla2 Rak2
;;;; 92 G#5 3301 Tabla2 Flam1
;;;; 93 A5  3302 Tabla2 Flam2
;;;; 94 A#5 3303 Tabla2 Tak Roll
;;;; 95 B5  3282 Djembe4 Dom1
;;;; 96 C6  3283 Djembe4 Dom2
;;;;

(in-package :modx)

(let* ((general-klist '((sagat-open    . (27 "D#0 3721 Sagat3 Open finger-cymbal"))
			(sagat-closed  . (25 "C#0 3720 Sagat2 Close"))
			(sagat-open2   . (29 "F0  3723 Sagat4 Open"))
			(sagat-closed2 . (26 "D0  3722 Sagat3 Close"))
			(sagat-closed3 . (28 "E0  3724 Sagat4 Close"))
			(mazhar-dom-open    . (30 "F#0 3826 Mazhar Dom Open"))
			(mazhar-dom-closed  . (31 "G0  3827 Mazhar Dom Close"))
			(mazhar-tak-open    . (33 "A0  3828 Mazhar Tak Open"))
			(mazhar-tak-closed  . (32 "G#0 3829 Mazhar Tak Close"))
			(mazhar-sak         . (34 "A#0 3830 Mazhar Sak Close"))
			(katem-dom    . (35 "B0  4052 Katem1 Dom1"))
			(katem-tak    . (36 "C1  4054 Katem1 Tak Open1"))
			(katem-dom2   . (37 "C#1 4053 Katem1 Dom2"))
			(katem-tak2   . (38 "D1  4055 Katem1 Tak Open2"))
			(katem-sak    . (39 "D#1 4056 Katem1 Sak1"))
			(tabel-dom    . (40 "E1  3337 Tabel Saaidi Dom"))
			(tabel-tak    . (42 "F#1 3338 Tabel Saaidi Tak"))
			(tabel-dom2   . (44 "G#1 3340 Tabel Dom"))
			(tabel-tak2   . (45 "A1  3341 Tabel Tak"))
			(tabel-tak3   . (46 "A#1 3342 Tabel Tak Wood"))
			(djembe-dom   . (95 "B5  3282 Djembe4 Dom1"))
			(djembe-dom2  . (96 "C6  3283 Djembe4 Dom2"))
			(djembe-tak   . (41 "F1  3284 Djembe4 Tak1"))
			(djembe-tak2  . (43 "G1  3285 Djembe4 Tak2"))
			(riq-dom    . (47 "B1  3770 Riq3 Dom Close"))
			(riq-dom2   . (48 "C2  3769 Riq3 Dom Open"))
			(riq-tak    . (49 "C#2 3772 Riq3 Tak Close1"))
			(riq-tak2   . (50 "D2  3771 Riq3 Tak Open"))
			(riq-tak3   . (51 "D#2 3773 Riq3 Tak Close2"))
			(riq-tak4   . (52 "E2  3774 Riq3 Tak Snouj"))
			(riq-sak    . (53 "F2  3768 Riq2 Sak"))
			(riq-snouj  . (54 "F#2 3775 Riq3 Snouj Close1"))
			(riq-snouj2 . (55 "G2  3776 Riq3 Snouj Close2"))
			(riq-flam   . (56 "G#2 3778 Riq3 Flam"))
			(riq-sak    . (57 "A2  3777 Riq3 Sak"))
			(riq-roll   . (58 "A#2 3779 Riq3 Snouj Roll"))
			(darbuka-dom    . (59 "B2  3963 Darbuka2 Dom LH"))
			(darbuka-dom2   . (60 "C3  3964 Darbuka2 Dom RH"))
			(darbuka-tak    . (61 "C#3 3966 Darbuka2 TakLIndxFng"))
			(darbuka-tak2   . (62 "D3  3967 Darbuka2 TakRIndxFng"))
			(darbuka-sak    . (63 "D#3 3965 Darbuka2 Sak1"))
			(darbuka-tak3   . (64 "E3  3968 Darbuka2 TakLRingFng"))
			(darbuka-tap    . (65 "F3  3970 Darbuka2 Tap1"))
			(darbuka-tap2   . (66 "F#3 3971 Darbuka2 Tap2"))
			(darbuka-flam   . (67 "G3  3972 Darbuka2 Flam1"))
			(darbuka-roll   . (68 "G#3 3974 Darbuka2 Tak Roll"))
			(zalgouta-A     . (69 "A3  4177 Arabic Zalgouta Opn"))
			(zalgouta-B     . (70 "A#3 4178 Arabic Zalgouta Cls"))
			(daholla-dom   . (71 "B3  4024 Daholla1 Dom LH"))
			(daholla-dom2  . (72 "C4  4025 Daholla1 Dom RH"))
			(daholla-tak   . (73 "C#4 4026 Daholla1 Tak1"))
			(daholla-tak2  . (74 "D4  4027 Daholla1 Tak2"))
			(daholla-tak3  . (75 "D#4 4028 Daholla1 Tak3"))
			(daholla-tak4  . (76 "E4  4029 Daholla1 Tak4"))
			(daholla-sak   . (77 "F4  4030 Daholla1 Sak"))
			(daholla-rak   . (78 "F#4 4031 Daholla1 Rak1"))
			(daholla-flam  . (80 "G#4 4033 Daholla1 Flam1"))
			(daholla-flam2 . (81 "A4  4034 Daholla1 Flam2"))
			(daholla-roll  . (82 "A#4 4035 Daholla1 Tak Roll"))
			(clap          . (79 "G4  2897 Arabic Hand Clap"))
			(tabla-dom-tak . (83 "B4  3292 Tabla2 Dom & Tak"))
			(tabla-dom     . (84 "C5  3293 Tabla2 Dom"))
			(tabla-tak     . (85 "C#5 3294 Tabla2 Tak1"))
			(tabla-tak2    . (86 "D5  3295 Tabla2 Tak2"))
			(tabla-tak3    . (87 "D#5 3296 Tabla2 Tak3"))
			(tabla-tak4    . (88 "E5  3297 Tabla2 Tak4"))
			(tabla-sak     . (89 "F5  3298 Tabla2 Sak"))
			(tabla-rak     . (90 "F#5 3299 Tabla2 Rak1"))
			(tabla-rak2    . (91 "G5  3300 Tabla2 Rak2"))
			(tabla-flam    . (92 "G#5 3301 Tabla2 Flam1"))
			(tabla-flam2   . (93 "A5  3302 Tabla2 Flam2"))
			(tabla-roll    . (94 "A#5 3303 Tabla2 Tak Roll"))))
       (sagat-klist (extract-sub-symbolic-keylist 'sagat general-klist))
       (mazhar-klist (extract-sub-symbolic-keylist 'mazhar general-klist))
       (katem-klist (extract-sub-symbolic-keylist 'katem general-klist))
       (tabel-klist (extract-sub-symbolic-keylist 'tabel general-klist))
       (djembe-klist (extract-sub-symbolic-keylist 'djembe general-klist))
       (riq-klist (extract-sub-symbolic-keylist 'riq general-klist))
       (darbuka-klist (extract-sub-symbolic-keylist 'darbuka general-klist))
       (zalgouta-klist (extract-sub-symbolic-keylist 'zalgouta general-klist))
       (daholla-klist (extract-sub-symbolic-keylist 'daholla general-klist))
       (clap-klist (extract-sub-symbolic-keylist 'clap general-klist))
       (tabla-klist (extract-sub-symbolic-keylist 'tabla general-klist))
       (nak1 (make-main-instrument new-arabic-kit-1 general-klist)))
  (make-sub nak1-sagat nak1 sagat-klist)
  (make-sub nak1-mazhar nak1 mazhar-klist)
  (make-sub nak1-katem nak1 katem-klist)
  (make-sub nak1-tabel nak1 tabel-klist)
  (make-sub nak1-djembe nak1 djembe-klist)
  (make-sub nak1-riq nak1 riq-klist)
  (make-sub nak1-darbuka nak1 darbuka-klist)
  (make-sub nak1-zalgouta nak1 zalgouta-klist)
  (make-sub nak1-daholla nak1 daholla-klist)
  (make-sub nak1-clap nak1 clap-klist)
  (make-sub nak1-tabla nak1 tabla-klist))

(export '(nak1-sagat
	  nak1-mazhar
	  nak1-katem
	  nak1-tabel
	  nak1-djembe
	  nak1-riq
	  nak1-darbuka
	  nak1-zalgouta
	  nak1-daholla
	  nak1-clap
	  nak1-tabla) :modx)

(import '(modx:nak1-sagat
	  modx:nak1-mazhar
	  modx:nak1-katem
	  modx:nak1-tabel
	  modx:nak1-djembe
	  modx:nak1-riq
	  modx:nak1-darbuka
	  modx:nak1-zalgouta
	  modx:nak1-daholla
	  modx:nak1-clap
	  modx:nak1-tabla) :cyco)

			
			
