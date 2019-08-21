;;;; controllers event clauses
;;;;
;;;; :time t1 t2    (allow *1 *2 substitution)
;;;; :time-to t     (allow *1 *2 substitution)
;;;; :value v1 v2   (allow *1 *2 substitution)
;;;; :value-to v    (allow *1 *2 substitution)
;;;; :steps n       0 < n < 128
;;;; :type n        (controller-number, controller-name, :pressure), may be list

(in-package :cyco-part)

(constant +make-controllers-docstring+ "
MAKE-CONTROLLERS creates a PART for control changes events.
It may generate MIDI control-change, channel-pressure and pitch-bend events.

NAME         - Symbol
INSTRUMENTS  - List of instruments or single instrument.
:SECTION     - Parent instrument, defaults to current section of *PROJECT*
:CUEFN       - Cueing function, defaults to section value
:SHIFT       - Float, time offset in seconds
:TEMPO       - Float, tempo in BPM, defaults to section value
:UNIT        - Symbol, time-signature unit, defaults to section value.
:BARS        - Integer, bar count, defaults to section value.
:BEATS       - Integer, beats per bar, defaults to section value.
:SUBBEATS    - Integer, subbeats per beat, defaults to section value.
:RENDER-once - Boolean, if true rendered events are not repeated within 
               section.
:CURVE       - Function used to alter curve shape.  It should take and return
               signed 'normalized' floats.  (lambda x) --> y
               -1.0 <= x <= +1.0,  -1.0 <= y <= +1.0
               Defaults to #'identity
:REMARKS     - String, optional remarks text
:EVENTS      - List of event specifications, see below.


Events list
Events are specified as a nested list of event 'clauses'.  Each clause 
begins with a keyword command and is followed by a prescribed number of 
arguments.

:events '((:command-1 arguments... :command-2 arguments... )  ...)

The available commands are

:TIME     - :time t1 t2
            Specifies time range of events, t1 and t2 must be in a format 
            accepted by the parts cuing function.   For the default BAR 
            function the format is a list (BAR BEAT SUBBEAT)
            The special symbols *1 and *2 may be substituted for the 
            previous t1 and t2 values respectively.

:TIME-TO  - :time-to t3
            Changes time range to t2 t3.
            time-to is equivalent to using time twice :time t1 t2  :time t2 t3
:VALUE    - :value v1 v2
            Specifies event values between v1 (at time t1) and v2 (at time t2).
            Values are 'normalized' floats  0.0 <= v <= +1.0
            For pitch-bend events they may be negative  -1.0 <= v <= +1.0
:VALUE-TO - :value-to v3
            Same as using value twice  :value v1 v2   :value v2 v3
:STEPS    - :steps n
            Number of generated events  2 <= n <= 128.
:TYPE     - :type v 
            Specifies event-type and may be one of the following
            bend      - pitch-bend
            pressure  - channel pressure
            integer   - MIDI controller number   0 <= n < 128
            other     - named MIDI controller, see GET-CONTROLLER-NUMBER.")

(constant +controllers-docstring+ (sformat "
CONTROLLERS has identical usage as MAKE-CONTROLLERS except it binds the new
part to the symbol name~%~%~A" +make-controllers-docstring+))
