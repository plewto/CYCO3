;;;; CYCO parts strummer-docs.lisp
;;;;
;;;; Documentation for STRUMMER class.
;;;;
;;;; STRUMMER is a type of part which produces strummed chords.  It has a
;;;; similar structure to SIMPLE-PART but provides much more flexibility.
;;;;
;;;; Mechanics:
;;;;
;;;;  STRUMMER extends PART with a list of CYCO-STRUMMER::STATE
;;;;  objects. STATE is a struct with the following fields.
;;;;
;;;;    source             - User supplied 'event' provided when the
;;;;                         STRUMMER instance is created.  This field
;;;;                         is only for error messages and debugging.
;;;;    time-specification - The current time specification used only for
;;;;                         debugging and testing.  The time-specification
;;;;                         format must satisfy the cuing function
;;;;                         requirements. 
;;;;    time               - float, offset time corresponding to
;;;;                         time-specification. 
;;;;    key                - key number
;;;;    chord-type         - chord name or template list.
;;;;    chord-inversion    - chord inversion degree.
;;;;    chord-octave       - chord added octaves.
;;;;    strum-delay        - time between successive strummed notes.
;;;;    strum-acceleration - acceleration of strum-delay.
;;;;    strum-direction    - defines pattern of how the chord template is
;;;;                         played. 
;;;;    strum-amp-scale    - amplitude scaling factor applied to successive
;;;;                         chord notes.
;;;;    strum-end-together - flag, if true all strummed notes end at the
;;;;                         same time, otherwise end times may be
;;;;                         staggered. 
;;;;    grace-key          - grace note key number.
;;;;    grace-delay        - offset of grace note from current time.
;;;;    grace-amp-scale    - relative amplitude of grace note.
;;;;    grace-articulation - grace note duration.
;;;;    articulation       - primary note duration.
;;;;    dynamic            - primary amplitude.
;;;;    dynamic-blur       - amplitude randomization amount.
;;;;    dynamic-min        - minimum allowed amplitude.
;;;;    dynamic-max        - maximum allowed amplitude.
;;;;    controller-number  - MIDI controller number.
;;;;    controller-value   - MIDI controller value.
;;;;    bend               - Pitch bend value.
;;;;    program-number     - program and bank numbers.
;;;;    program-bank       -
;;;;
;;;; Upon construction an initial state object is created.  As the list 
;;;; passed to the :events argument is evaluated, various fields of the
;;;; state are updated.  Some of these fields are persistent and remain in
;;;; effect until explicitly changed.  When any of the following fields
;;;; are set,  the state object is cloned and added to the strummer-states
;;;; list:
;;;;
;;;;    key, grace-key, controller-number, bend, program-number,
;;;;    program-bank. 
;;;; 
;;;;

(in-package :cyco-part)

(constant +make-strummer-docstring+
"Creates an instance of STRUMMER, a type of PART for producing strummed chords. 

name          - Part name.
instrument    - Single instance of INSTRUMENT.
:section      - Parent section, defaults to current section of *PROJECT*
:cuefn        - The cuing function, defaults to parent section value.
:shuffle      - The shuffle function, defaults to parent section value.
:shift        - Metric-expression, Time shift added to each event, default 0.
:tempo        - Temp in BPM, defaults to parent section value.
:unit         - Time signature unit, defaults to parent section value.
:bars         - Length in bars, defaults to parent section value.
:beats        - Beats per bar, defaults to parent section value.
:subbeats     - Sub-beats per beat, defaults to parent section value.
:render-once  - Boolean, if true the part is not repeated with its section.
:transposable - Boolean, if true the part is subject to transposition and
                key inversion.
:reversible   - Boolean, if true the part is subject to being reversed by
                retrograde. 
:chord-model  - Defaults to parent section.
:remarks      - Optional remarks text.
:events       - List of event specifications, see below.


Events are specified as a nested list of event 'clauses' where each clause
begins with a keyword and is followed by a specific number of arguments.
Clauses fall into two general classes:

   1) Those that set some 'environmental' value.  These values remain in
      effect until explicitly changed.

   2) Those which generate events.  Once the events are generated these
      clauses are cleared.  The event clauses are  :key, :grace-key, :bend, 
      :controller, :program and :bank.

The available clause types are detailed below.


:reset          - (No arguments) Sets all parameters to default values.

:time           - :time specification
                  Sets event time.  The specification format must match
                  whatever the cuing function expects.  For the default 
                  BAR cuing function the format is a list (BAR BEAT SUBBEAT).

:chord          - :chord type
                  Designates which chord to play.  Chords are either
                  specified by name or as a list of keynumbers or keynumber
                  offsets.  

                  1) By name.  If a name is supplied it must be defined by
                     the part's chord-model.   Typical examples are [solo], 
                     [maj] and [min]  for single note, major and minor
                     chords respectively.  See ?chords function for list of
                     available chords.

                  2) By list.
                     A) The chord-model is 'absolute'. 
                        Some chord-models, particularly simulations of
                        fretted instruments, are designated as absolute.  
                        When using an absolute chord-model the list
                        provided to :chord is taken as a literal set of
                        keynumbers.  The actual key-number specified by the
                        :key clause is ignored.

                     b) For non-absolute chord models, which is the
                        default,  the list provided to :chord is treated as
                        a list of key-number offsets relative to the value
                        of the :key clause.   The following line plays a
                        c-major chord

                        (:key c5 :chord (0 4 7))

:inversion      - :inversion degree    -12 <= degree <= +12
:inv              Set the chord inversion degree.  See CHORD-INVERSION
                  function. 

                  :inversion and :inv are identical.

:octave         - :octave n   -3 <= n <= +3
:oct              Sets added chord octave.  See CHORD-INVERSION function.

                  :octave and :oct are identical.

:strum          - :strum delay
                  Sets delay time of successive chord notes.  delay may
                  either be in absolute seconds or a metric expression.

:strum*         - :strum acceleration     0.1 <= acceleration <- 8.0
                  Adds an acceleration to strum delay times.

:direction      - :direction direction
                  :direction list
                  Selects the order note are played from the chord
                  template.   Valid directions are:   down, up, dice and
                  random.   

                  down   - play notes in order.
                  up     - reverse note order.
                  dice   - selects up or down randomly.
                  random - play notes in a random order.

                  If a list of directions is specified it must only
                  contain these four symbols.  List are used in a cycle 
                  over the next n chords.

:end-together   - :end-together no|yes
                  If yes all note in a chord have the same end-time.  If no
                  the end times may be staggered. 

:amp*           - :amp* scale   0.25 <= scale <= 4.0
                  Applies an amplitude scale to to successive strummed notes.

:grace-amp*     - :grace-amp* scale   0.1 <= scale <= 4.0
                  Sets grace-note relative amplitude.

:grace-duration - :grace-duration number|metric-expression
                  Sets grace note duration.

:grace-delay    - :grace-delay time
                  Sets grace-note offset relative to current time.  The
                  time argument may either be in seconds or a
                  metric-expression.   Negative values move the grace-note
                  before the primary notes, while positive values push
                  grace notes after the main note.  

:dur            - :dur seconds|metric-expression
                  Sets primary note duration, either in absolute seconds or
                  by a relative metric-expression.

:amp            - :amp a
                  :amp list
                  Sets the reference note amplitude, either as a single
                  dynamic value or as a list of dynamic values.  When a
                  list is used it is taken as a pattern of n amplitudes which
                  repeats over the next n events.

:cres           - :cres start end count
                  Sets up a crescendo or decrescendo amplitude pattern.
                  The start and end values have the same usage as with :amp
                  clauses.  Once the final amplitude is reached, it remains
                  in effect until explicitly changed.
 
:amp-blur       - :amp-blur n   0 <= n <= 1
                  Applies randomization to amplitude values.  

:amp-limits     - :amp-limits min max
                  Sets amplitude range limits.

:key            - :keyword key
                  Generates the key events in conjunction with the
                  current chord/strum values.  The key number is first
                  interpreted by the instrument's keynumber-map.  If an
                  'absolute' chord-model is in use the key Selects the
                  chord variation.  

:grace          - :grace key 
                  Produces a grace note.  Grace notes are always single
                  notes which ignore the current chord.

:bend           - :bend n    -1.0 <= n <= +1.0
                  Produces a single bend event.

:cc             - :cc controller-number value
                  Produces a single control-change event
                  0 <= controller-number <= 127
                  0.0 <= value <= 1.0

:program        - :program p
:bank           - :bank b p   
                  Pass p and b arguments to instrument's program-map 
                  function.")



(constant +strummer-docstring+
       (sformat "~A~%~%~A"
		"The STRUMMER macro has the same usage as MAKE-STRUMMER
       except it binds the result to a symbol with the same name as the
       strummer."
		+make-strummer-docstring+))
		
