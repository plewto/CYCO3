;;;; Project test-1 performs the following task:
;;;;   1. Creates project
;;;;   2. loads 'orchestra' file
;;;;      A) Loads general-midi plugin
;;;;      B) Prunes orchestra
;;;;      C) Defines a few instruments
;;;;   3. Loads prerunn section file
;;;;   4. Loads Alpha section file
;;;;   5. Set section order
;;;;   6. Render project and dump generated events.
;;;;

(project test-1 :tempo 60 :bars 4 :beats 4
	 :project-directory (join-path *test-project-directory* "test-1"))

(lpf 'orchestra)
(lpf 'preroll)
(lpf 'alpha)
