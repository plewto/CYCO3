;;;; CYCO modx-drums TURKISH-KIT
;;;; May be used with TURKISH-KIT or TURKISH-ST-KIT
;;;;
;;;; 24 3217 Claves1               :
;;;; 25 3573 AsmaDavul L Side 1-2  :
;;;; 26 3577 AsmaDavul R Side 1-2  :
;;;; 27 3570 AsmaDavulSideBdy 1-2  :
;;;; 28 3576 AsmaDavul Both Sides  :
;;;; 29 3591 KoltukDavulOpenFlam   :
;;;; 30 3583 KoltukDavulTekeOp1-3  :
;;;; 31 3587 KoltukDavulTekOp 1-3  :
;;;; 32 3580 Koltuk Davul Dum 1-2  :
;;;; 33 3626 Bendir Teke OpenFlam  :
;;;; 34 3633 Bendir1 Teke Dead1-2  :
;;;; 35 3630 Bendir1 Tek Dead1-2   :
;;;; 36 3622 Bendir Teke 1-3       :
;;;; 37 3618 Bendir Tek 1-3        :
;;;; 38 3627 Bendir Slap 1-2       :
;;;; 39 3615 Bendir Dum 1-2        :
;;;; 40 3713 Sagat1 R Close 1-2    :
;;;; 41 3712 Sagat1 R Open         :
;;;; 42 3717 Sagat1 L Close 1-2    :
;;;; 43 3716 Sagat1 L Open         :
;;;; 44 3743 Riq1 Teke Flam        :
;;;; 45 3737 Riq1 Tek Mute 1-2     :
;;;; 46 3740 Riq1 Teke Damped 1-2  :
;;;; 47 3744 Riq1 TekMtMed 1-2     :
;;;; 48 3730 Riq1 Dum Mute         :
;;;; 49 3754 Riq1 Cymbal 1-2       :
;;;; 50 3757 Riq1 Cymbal Mute 1-2  :
;;;; 51 3767 Riq1 Tremolo          :
;;;; 52 3761 Riq1 ShakeA 1-2       :
;;;; 53 3764 Riq1 ShakeB 1-2       :
;;;; 54 3753 Riq1 Tek Flam         :
;;;; 55 3760 Riq1 Full Open        :
;;;; 56 3750 Riq1 TekeOpnShrt 1-2  :
;;;; 57 3747 Riq1 TekOpnShrt 1-2   :
;;;; 58 3734 Riq1 Tek Open 1-2     :
;;;; 59 3731 Riq1 Dum Open 1-2     :
;;;; 60 3837 Hollo FingerDead 1-2  :
;;;; 61 3834 Hollo Slap 1-2        :
;;;; 62 3831 Hollo Dum 1-2         :
;;;; 63 4114 Kasik 1-2             :
;;;; 64 4118 Kasik Flam            :
;;;; 65 3858 Tombek Tek Dead Sw    :
;;;; 66 3860 Tombek Tek Flam       :
;;;; 67 3864 Tombek Teke 1-3       :
;;;; 68 3854 TombekTekeOthFng 1-3  :
;;;; 69 3850 TombekTekeIndFng 1-3  :
;;;; 70 3846 Tombek Tek 1-3        :
;;;; 71 3843 Tombek Slap 1-2       :
;;;; 72 3861 Tombek Slap Med 1-2   :
;;;; 73 3840 Tombek Dum 1-2        :
;;;; 74 3950 Darbuka1 Roll Close   :
;;;; 75 3948 Darbuka1 RollOpen     :
;;;; 76 3949 Darbuka1 TekeDmpFlam  :
;;;; 77 3951 Darbuka1 TekDead 1-3  :
;;;; 78 3955 Darbuka1 TekDampd1-3  :
;;;; 79 3947 Darbuka1 TekeOpnFlam  :
;;;; 80 3959 Darbuka1 TekeOpen1-3  :
;;;; 81 3933 Darb1TekeOthFngA 1-3  :
;;;; 82 3925 Darb1TekeIndFngA 1-3  :
;;;; 83 3917 Darbuka1 Teka1 1-3    :
;;;; 84 3937 Darb1TekeOthFngB 1-3  :
;;;; 85 3929 Darb1TekeIndFngB 1-3  :
;;;; 86 3921 Darbuka1 TekB 1-3     :
;;;; 87 3944 Darbuka1 SlapMed 1-2  :
;;;; 88 3941 Darbuka1 Slap 1-2     :
;;;; 89 3914 Darbuka1 Dum 1-2      :
;;;; 90 4127 Bonges Tek Roll       :
;;;; 91 4128 Bonges Flam           :
;;;; 92 4126 Bonges Tek Flam       :
;;;; 93 4122 Bonges Tek 1-2        :
;;;; 94 4125 Bonges Slap           :
;;;; 95 4129 Bonges Flam Hi        :
;;;; 96 4119 Bonges Dum 1-2        :

(in-package :modx)

(let* ((general-klist '((claves        . (24 "3217 Claves1"))
			(davul-left    . (25 "3573 AsmaDavul L Side 1-2"))
			(davul-right   . (26 "3577 AsmaDavul R Side 1-2"))
			(davul-body    . (27 "3570 AsmaDavulSideBdy 1-2"))
			(davul-both    . (28 "3576 AsmaDavul Both Sides"))
			(davul2-left   . (32 "3580 Koltuk Davul Dum 1-2"))
			(davul2-right  . (30 "3583 KoltukDavulTekeOp1-3"))
			(davul2-right2 . (31 "3587 KoltukDavulTekOp 1-3"))
			(davul2-flam   . (29 "3591 KoltukDavulOpenFlam"))
			(bendir-open  . (39 "3615 Bendir Dum 1-2"))
			(bendir-tek   . (36 "3622 Bendir Teke 1-3"))
			(bendir-tek2  . (37 "3618 Bendir Tek 1-3"))
			(bendir-slap  . (38 "3627 Bendir Slap 1-2"))
			(bendir-mute  . (34 "3633 Bendir1 Teke Dead1-2"))
			(bendir-mute2 . (35 "3630 Bendir1 Tek Dead1-2"))
			(bendir-flam  . (33 "3626 Bendir Teke OpenFlam"))
			(sagat-open-r   . (41 "3712 Sagat1 R Open"))
			(sagat-closed-r . (40 "3713 Sagat1 R Close 1-2"))
			(sagat-open-l   . (43 "3716 Sagat1 L Open"))
			(sagat-closed-l . (42 "3717 Sagat1 L Close 1-2"))
			(riq-open        . (55 "3760 Riq1 Full Open"))
			(riq-tek-mute    . (45 "3737 Riq1 Tek Mute 1-2"))
			(riq-tek-mute2   . (46 "3740 Riq1 Teke Damped 1-2"))
			(riq-tek-mute3   . (47 "3744 Riq1 TekMtMed 1-2"))
			(riq-dum-mute    . (48 "3730 Riq1 Dum Mute"))
			(riq-cym         . (49 "3754 Riq1 Cymbal 1-2"))
			(riq-cym2        . (50 "3757 Riq1 Cymbal Mute 1-2"))
			(riq-shake       . (53 "3764 Riq1 ShakeB 1-2"))
			(riq-shake2      . (52 "3761 Riq1 ShakeA 1-2"))
			(riq-tek-flam    . (54 "3753 Riq1 Tek Flam"))
			(riq-tek-open    . (56 "3750 Riq1 TekeOpnShrt 1-2"))
			(riq-tek-open2   . (57 "3747 Riq1 TekOpnShrt 1-2"))
			(riq-tek-open3   . (58 "3734 Riq1 Tek Open 1-2"))
			(riq-dum-open    . (59 "3731 Riq1 Dum Open 1-2"))
			(riq-tremolo     . (51 "3767 Riq1 Tremolo"))
			(riq-flam        . (44 "3743 Riq1 Teke Flam"))
			(hollo-open  . (62 "3831 Hollo Dum 1-2"))
			(hollo-slap  . (61 "3834 Hollo Slap 1-2"))
			(hollo-mute  . (60 "3837 Hollo FingerDead 1-2"))
			(kasik      . (63 "4114 Kasik 1-2"))
			(kasik-flam . (64 "4118 Kasik Flam"))
			(tombek-dum        . (73 "3840 Tombek Dum 1-2"))
			(tombek-tek        . (68 "3854 TombekTekeOthFng 1-3"))
			(tombek-tek2       . (69 "3850 TombekTekeIndFng 1-3"))
			(tombek-tek3       . (70 "3846 Tombek Tek 1-3"))
			(tombek-tek-mute   . (65 "3858 Tombek Tek Dead Sw"))
			(tombek-tek-flam   . (66 "3860 Tombek Tek Flam"))
			(tombek-tek4       . (67 "3864 Tombek Teke 1-3"))
			(tombek-slap       . (71 "3843 Tombek Slap 1-2"))
			(tombek-slap-mute  . (72 "3861 Tombek Slap Med 1-2"))
			(darbuka-dum         . (89 "3914 Darbuka1 Dum 1-2"))
			(darbuka-tek-mute    . (77 "3951 Darbuka1 TekDead 1-3"))
			(darbuka-tek-mute2   . (78 "3955 Darbuka1 TekDampd1-3"))
			(darbuka-tek-flam    . (79 "3947 Darbuka1 TekeOpnFlam"))
			(darbuka-tek-open    . (80 "3959 Darbuka1 TekeOpen1-3"))
			(darbuka-tek         . (81 "3933 Darb1TekeOthFngA 1-3"))
			(darbuka-tek2        . (82 "3925 Darb1TekeIndFngA 1-3"))
			(darbuka-tek3        . (83 "3917 Darbuka1 Teka1 1-3"))
			(darbuka-tek4        . (84 "3937 Darb1TekeOthFngB 1-3"))
			(darbuka-tek5        . (85 "3929 Darb1TekeIndFngB 1-3"))
			(darbuka-tek6        . (86 "3921 Darbuka1 TekB 1-3"))
			(darbuka-slap        . (87 "3944 Darbuka1 SlapMed 1-2"))
			(darbuka-slap2       . (88 "3941 Darbuka1 Slap 1-2"))
			(darbuka-tek-flam2   . (76 "3949 Darbuka1 TekeDmpFlam"))
			(darbuka-roll-open   . (75 "3948 Darbuka1 RollOpen"))
			(darbuka-roll-closed . (74 "3950 Darbuka1 Roll Close"))
			(bonges-dum          . (96 "4119 Bonges Dum 1-2"))
			(bonges-tek          . (93 "4122 Bonges Tek 1-2"))
			(bonges-slap         . (94 "4125 Bonges Slap"))
			(bonges-flam         . (95 "4129 Bonges Flam Hi"))
			(bonges-flam2        . (91 "4128 Bonges Flam"))
			(bonges-tek-flam     . (92 "4126 Bonges Tek Flam"))
			(bonges-tek-roll     . (90 "4127 Bonges Tek Roll"))))

       (claves-klist (extract-sub-symbolic-keylist 'claves general-klist))
       (davul-klist (extract-sub-symbolic-keylist 'davul general-klist))
       (davul2-klist (extract-sub-symbolic-keylist 'davul2 general-klist))
       (bendir-klist (extract-sub-symbolic-keylist 'bendir general-klist))
       (sagat-klist (extract-sub-symbolic-keylist 'sagat general-klist))
       (riq-klist (extract-sub-symbolic-keylist 'riq general-klist))
       (hollo-klist (extract-sub-symbolic-keylist 'hollo general-klist))
       (kasik-klist (extract-sub-symbolic-keylist 'kasik general-klist))
       (tombak-klist (extract-sub-symbolic-keylist 'tombak general-klist))
       (darbuka-klist (extract-sub-symbolic-keylist 'darbuka general-klist))
       (bonges-klist (extract-sub-symbolic-keylist 'bonges general-klist))
       (trk (make-main-instrument turkish-kit general-klist)))
  (make-sub trk-claves trk claves-klist)
  (make-sub trk-davul trk davul-klist)
  (make-sub trk-davul2 trk davul2-klist)
  (make-sub trk-bendir trk bendir-klist)
  (make-sub trk-sagat trk sagat-klist)
  (make-sub trk-riq trk riq-klist)
  (make-sub trk-hollo trk hollo-klist)
  (make-sub trk-kasik trk kasik-klist)
  (make-sub trk-tombak trk tombak-klist)
  (make-sub trk-darbuka trk darbuka-klist)
  (make-sub trk-bonges trk bonges-klist))

(export '(trk-claves
	  trk-davul
	  trk-davul2
	  trk-bendir
	  trk-sagat
	  trk-riq
	  trk-hollo
	  trk-kasik
	  trk-tombak
	  trk-darbuka
	  trk-bonges) :modx)

(import '(modx:trk-claves
	  modx:trk-davul
	  modx:trk-davul2
	  modx:trk-bendir
	  modx:trk-sagat
	  modx:trk-riq
	  modx:trk-hollo
	  modx:trk-kasik
	  modx:trk-tombak
	  modx:trk-darbuka
	  modx:trk-bonges) :cyco)


