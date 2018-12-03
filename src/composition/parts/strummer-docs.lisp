
(constant +strummer-documentation+
"Creates new instance of STRUMMER, a part type with explicit event
specifications.

name          - Symbol, part name
instruments   - Instrument, single instrument only.
:section      - Parent section, defaults to current-section of *project*
:cuefn        - Cueing-function, default to parent value.
:shift        - Offset time for initial event.  The format of shift is
                dependent on cuefn.  When rendering an Strummer within a
                section, the first iteration is offset by shift amount.
                Defaults to 0.0.
:render-once  - Flag, if true, render-n only renders strummer events one time. 
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

 A Strummer may generate note events and has a rich set of options for
 strumming chords.   They may also generate simple touch, MIDI controller,
 bend and program events.
 
 Events are specified as a nested list of event commands.
 
     ((:command n :command n ...)
      (:command n :command n ...)
      ...........................)
 
 Where each clause within a single event sub-list has the form
 
      :command [arguments..]
 
 and :command is one of the keywords below. Argument counts are between 
 0 and 3, depending on the specific command.
 
 :TRACE off
 :TRACE events
 :TRACE states
 :TRACE all     
     Turns debug trace mode on/off 
 
 :BREAK
     Skips evaluation of remaining events.
 
 :STOP
     Skips remainder of current event.
 
 :RESET
     Resets all parameters to initial condition.

 :TIME argument
 :TIME argument-list
     Specifies event time.  The accepted format is dependent on
     the part's cuing-function.  Time remains in effect for all future
     events until either:
     1) It is explicitly changed.
     3) On reset.
 
 :KEY k
     Creates a note event.
     The k argument is typically a keynumber but it need not be so long as
     the instrument's keynumber-map is able to convert it to a keynumber.
 
     Keynumbers are not saved between events.
 
 :CHORD name
 :CHORD template
     Selects chord type, either by name or as a list of key offsets.
 
     :chord [maj]   
     :chord (0 4 7)
 
     Both select a simple major triad (assuming the part's chord-model
     defines [maj]).  The chord remains in effect until explicitly
     changed.
     
 :INVERSION n
     Sets chord inversion, n should be a small signed integer.
     Positive values rotate the chord template right.
 
     (0 3 7)  inversion 0  --> (0 3 7)
     (0 3 7)  inversion 1  --> (3 7 12)
     (0 3 7)  inversion 2  --> (7 12 15)
     (0 3 7)  inversion -1 --> (-5 0 3)
 
     Inversion remains in effect until explicitly changed.
 
 :OCTAVE n
     Inserts new note into chord template n-octaves above the initial
     note. 
 
     (0 3 7)  octave 0  --> (0 3 7)
     (0 3 7)  octave 1  --> (0 3 7 12)
     (0 3 7)  octave -1 --> (0 3 7 -12)
 
     Octave is applied after inversion and remains in effect until
     explicitly changed. 
 
 :STRUM metric-expression
     Sets strum delay time between each note of a chord, I.E. strums the chord.
 
     For delay d and time t, chord notes are produced at times
     t, t+d, t+2d, t+3d, ...
 
     strum delay remains in effect until explicitly changed.
 
 :STRUM* scale
     Adds strum delay acceleration.

     For scale s, delay d, and time t, chord notes are produced at times:
     t, t+d, t+d*s, t+d*s^2, t+d*s^3, ...
 
     Acceleration remains in effect until explicitly changed. 
    
 :DIRECTION d
 :DIRECTION list 
     Sets strum direction.
 
     Valid directions are
         :down - play chord in note order
         :up   - reverse note order
         :dice - randomly select :up or :down
         :random - select notes randomly.
 
     The strum direction is a Cycle pattern
 
     :DIRECTION :down
     Play all chords in note order.
 
     :DIRECTION (:down :up :dice)
     Play first chord in note order.
     Play every second chord in reverse order.
     Randomly select :up or :down for every third chord.
 
     Direction remains in effect until explicitly changed.

 :GRACE key delay 
    Play grace note
    key    - key number
    delay  - metric expression
    Grace notes ignore chord-template

 :GRACE-AMP* n
    Scale normal dynamic by n for the grace note.

 :GRACE-DUR ex
    Sets grace note articulation where ex is a metric-expression

 :AMP* s
     Scale dynamic of each chord note by s.
     For nominal dynamic a and scale s, note amplitudes are:
 
     a, a*s, a*s^2, a*s^3, ...
 
     See also :AMP-RANGE and :AMP-BLUR.
     AMP* remains in effect until explicitly changed.
 
 :END-TOGETHER no
 :END-TOGETHER yes
     Selects if strummed chord notes are to end at the same time or have
     staggered endings.
 
     end-together remains in effect until explicitly changed.
 
 :DUR metric-expression|number
     Sets note duration.
     Duration remains in effect until explicitly changed.
     If duration is a non-negative number it is used as the absolute
     note duration (subject to the instrument articulation maps).
     If the duration is a metric-expression, it's value is scaled by 
     the current tempo.

 :AMP dynamic
 :AMP list  (converted to cycle)
    Sets nominal note dynamic level pattern
    Dynamic level remains in effect until explicitly changed.

 :CRES start end count
    Creates dynamic change from start to end over the next
    count events.  start and end are dynamic values, count is
    an integer.  The special value of '= for start indicates
    the current amplitude.

 :AMP-BLUR s
    Randomizes dynamic values for each new key-event
    A value of s=0.1 blurs dynamic by up to 10 percent.
 
    Blur remains in effect until explicitly changed.
 
 :AMP-RANGE min max
    Sets minimum and maximum dynamic values. Limiting is applied after amp+,
    amp*, amp-blur and any strum scaling.  Both min and max are dynamic
    values and remain in effect until explicitly changed.
 
 :TOUCH n
    Creates a single MIDI after-touch event 
    0.0 <= n <= 1.0
 
 :BEND b
    Creates a single MIDI bend event 
    -1.0 <= b <= +1.0
 
 :CTRL c
    Sets MIDI controller number.
    Where c is an integer between 0 and 127 inclusive.
    The controller number remains in effect until explicitly changed.
 
 :CC n
   Creates single MIDI controller event 
   controller number established by CTRL.
 
   0.0 <= n <= 1.0
 
 :PROGRAM p
    Creates simple program-change events 
    The expected format for p is dependent on the instruments
    program-map functions.
 
    A value of default should cause the instrument to generate it's
    default programs.   Numeric p values are typically interpreted as
    explicit MIDI program numbers.	  

 :BANK b p
    Creates bank and program-change events
    The expected format for bank b and program p is dependent
    on the instruments program-map functions.
 
    A value of default should cause the instrument to generate it's
    default bank/programs.   Numeric p values are typically interpreted as
    explicit MIDI program numbers.	  
")
