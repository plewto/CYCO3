
(constant +strummer-documentation+
"Creates new instance of STRUMMER, a part type with emphasis on strumming
chords.

name          - Symbol, part name
instruments   - Instrument, single instrument only.
:section      - Parent section, defaults to current-section of *project*
:cuefn        - Cueing-function, default to parent value.
:shift        - Offset time for initial event.  The format of shift is
                dependent on cuefn.  When rendering an Strummer within a
                section, the first iteration is offset by shift amount.
                Defaults to 0.0.
:render-once  - Flag, if true, the part is not repeated after it's initial events.
:transposable - Flag, if true this part is subject to transpose and invert.
:reversible   - Flag, if true this part is subject to retrograde operations.
:chord-model  - Establish chord-model, defaults to chord-model of 
                parent section.
:remarks      - String, remarks text.
:events       - See below
:tempo        - Time signature tempo, defaults to parent's value.
:unit         - Time signature unit, defaults to parent's value.
:bars         - Time signature bars, defaults to parent's value.
:beats        - Time signature beats, defaults to parent's value.
:subbeats     - Time signature subbeats, defaults to parent's value. 

Strummer has a heavy emphasis on keyed events, particularly for playing 
chords.  It may also produce very simple bend, controller and program
events.  

 Events are specified as a nested list of event commands.
 
     ((:command n :command n ...)
      (:command n :command n ...)
      ...........................)
 
 Where each 'clause' within a sub-list has the form
 
      :command [arguments..]
 
 and :command is one of the keywords below. Argument counts are between 
 0 and 3, depending on the specific command.  Commands fall into three
 general categories: 

      1) Test 
         :TRACE :BREAK

      2) Parameter values.  Most commands set a parameter value such as 
         note amplitude or duration.  These do not produce any any events
         and remain in effect until explicitly changed.
         :RESET :TIME :CHORD :INVERSION :OCTAVE :STRUM :STRUM* :DIRECTION
         :AMP* :END-TOGETHER :GRACE-AMP* :GRACE-DURATION :GRACE-DELAY
         :DUR :AMP :CRES :AMP-BLUR :AMP-LIMITS

      3) Generators, commands which actually create MIDI events
         :KEY :GRACE :BEND :CC :PROGRAM :BANK
 
:TRACE-ON   
:TRACE-OFF
    Toggle diagnostic trace mode.

:BREAK
    Skips evaluation of remaining events.

:RESET 
   Sets all parameters to initial defaults.

:TIME (args...)
   Specifies event time in terms of the time-signature and cue-function.
   The exact format is dependent on cue-function.

:CHORD name
:CHORD list
   Specifies chord type, either by name or as a list of key-number
   offsets.  A list of offsets is always relative to the current key
   number, i.e. (0 4 7) is a major triad.  Named chords must be defined
   by the chord-model in effect.  By convention chord names are enclosed
   in square brackets  [maj], though there are exceptions.  Use 
   (?chords chord-model) to print a list of defined chords.  

   Use (0) to play single notes.

:INVERSION n
   Sets chord inversion.  The inversion is produced by rotating the chord
   template n-times and transposing the rotated-value by an octave.
   Positive inversions produce higher sounding chords.  

    (0 4 7) inversion  0 -> (0 4 7)
            inversion  1 -> (4 7 12)
            inversion  2 -> (7 12 16)
            inversion  3 -> (12 16 19) 
            inversion -1 -> (-5 0 4)
            inversion -2 -> (-8 -5 0)

:OCTAVE n
   Modifies chord template by appending an octave copy of the first note to
   the end.  -3 <= n <= +3

   (0 4 7) octave -1 -> (0 4 7 -12)
           octave  0 -> (0 4 7)
           octave  1 -> (0 4 7 12)

:STRUM delay
   Sets delay time between articulation of each chord note.  The delay time
   may be in absolute seconds, or a metric-expression

:STRUM* scale
   Sets strum acceleration, 0.125 <= scale <= 8.00, default 1.0 = no
   acceleration.

:DIRECTION d
:DIRECTION list
   Sets strum direction.  Possible values are:
      down   - play chord forward.
      up     - reverse note order.
      dice   - randomly selects up or down.
      random - randomize note order.

   If the argument is a list it is treated as a cycle pattern and each
   successive note uses the next direction the list.

:AMP* s
   Scales amplitude of successive chord notes by s.  0.25 <= s <= 4.0

:END-TOGETHER no
:END-TOGETHER yes
   Selects timing of chord notes.  If no note endings are staggered in the 
   same order they were 'strummed'.

   If yes, all notes end at the same time.

:GRACE-AMP* s
   Sets grace-note amplitude scale   0.25 <= s <= 4.0

:GRACE-DURATION ex
   Sets grace-note duration, either in absolute seconds or by a relative
   metric-expression.

:GRACE-DELAY dly
   Sets delay time for producing grace notes relative to the current time.
   Delay argument may be either in absolute seconds or a relative
   metric-expression. 

:DUR d
   Note duration either in absolute seconds or as a relative
   metric-expression. 

:AMP a
:AMP list
   Note amplitude pattern.   If the argument is a list it is treated as
   cycle pattern which advances with each new note.

:CRES start end count
   Produce a crescendo/decrescendo over the next count notes, count must not
   be 0.   start and end are the initial and final amplitudes respectively.
   The crescendo is implemented as a Line Pattern, once the final value has
   been reached,it remains in effect until explicitly changed. 

:AMP-BLUR s
   Sets blurring factor to amplitude 0.0 <= s <= 1.0.
   A value of 0.0 disables blurring.  A value of 1.0 completely randomizes
   amplitudes. 
   
:AMP-LIMIT min max
   Sets range of allowed amplitudes.  Amplitudes outside the interval
   (min,max) are clipped. 

:KEY k
   Generate a key event.  Key-number k is processed by the instrument's
   keynumber-map prior to being used.  The key is expanded using the
   current chord/strum settings.

:GRACE k
    Generates grace-note event.  Grace notes are not expanded by the
    current chord.

:BEND b
    Produce a single pitch-bend event.  -1.0 <= b <= +1.0

:CC ctrl n
   Produce a single MIDI controller event. 
   ctrl - the controller number.
   n - the normalized value.  0 <= n < 1.

:PROGRAM p
   Generate program-change event(s).
   If p argument is the symbol DEFAULT, the instrument's default program is
   used.   Otherwise p is interpreted by the instrument's program-change
   map.

:BANK b p
   Generate program-change events with bank change.
   Either bank b or program p arguments may have special value DEFAULT.")
