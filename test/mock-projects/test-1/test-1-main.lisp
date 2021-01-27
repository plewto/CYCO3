;;;; Project test-1 performs the following task:
;;;;   1. Creates project
;;;;   2. loads 'orchestra' file, checks that instruments are:
;;;;       * bound to named symbol
;;;;       * have correct MIDI channels
;;;;       * partial test of orchetra tree structure.
;;;;
;;;;   3. Loads prerun section file, checks:
;;;;       * section was creted and bound to named symbol.
;;;;       * expected program-change and metronome events are generated.
;;;;
;;;;   4. Loads Alpha section file


(version 3)

(project test-1 :tempo 60 :bars 4 :beats 4
	 :project-directory (join-path *mock-project-directory* "test-1"))

(plugin general-midi)

(lpf orchestra)
(lpf preroll)

