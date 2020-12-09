;;;; CYCO parts control-ball-docs.lisp
;;;;
;;;; Defines docstring for control-ball.
;;;; A CONTROL-BALL is a PART for generating MIDI controller, channel-pressure and
;;;; bend events.
;;;;

(in-package :cyco-part)

(constant +control-ball-docstring+
	  "Creates new instance of CONTROL-BALL.
name        - symbol, part name
controller  - MIDI controller number (0..127) inclusive, :PRESSURE or :BEND
instruments - Instrument or list of instruments.
start       - Start time, must be in a format accepted by cuefn.
end         - End time, must be in a format accepted by cuefn.
:section    - Parent section, defaults to current section of *PROJECT*
:cuefn      - Time cue function, defaults to section value.
:shuffle    - Time shuffle function, defaults section value.
:shift      - metric-expression, time offset added to each event, default 0.0
:tempo      - Defaults to parent section value.  
:unit       - Defaults to parent section value.
:bars       - Defaults to parent section value.
:beats      - Defaults to parent section value.
:subbeats   - Defaults to parent section value.
:interval   - Metric-expression, default 's
:pattern    - Value pattern or pattern specification, see below, default nil.
:trim       - Integer, number of events to remove from end of pattern, default 0.
:initial    - cons (value shift), default nil
:final      - cons (value shift), default nil
:remarks    - Optional remarks text
:render-once     - Boolean, if true do not repeat, default false.
:reset-on-repeat - Boolean, if true reset pattern on repeat, default t.

;; The pattern argument specifies the controller values to be generated and must 
;; either be nil or a PATTERN which produces numbers only.   All data values
;; are 'normalized'  0.0 <= value <= 1.0  for controller and pressure and
;; -1.0 <= value <= +1.0 for bend.  Out of range values are clipped.  

The pattern argument specifies the controller values to be generated and may 
be one of the following:

   1) nil (the default), do not generate events, does not apply to initial 
      and final arguments.

   2) A pattern object.  The pattern must only produce numeric values.
      All values are normalized 0.0 <= value <= 1.0 for controller and
      pressure, -1.0 <= value <= +1.0 for bend.  Out of bounds values
      are clipped.

   3) A pattern specification list of form 

      (type args...)

      Where type is one of :RAMP  :SAWTOOTH  :TRIANGLE  or :PULSE.

      With remaining arguments required by the ramp, sawtooth, triangle
      and pulse functions respectively.   The steps value is automatically
      determined.

The initial and final arguments specify single events to be generated
prior to the start time and after the end time, respectively.  Both
must either be a cons (value shift) or nil.  shift value maybe 
absolute in seconds or a metric-expression.

:initial (cons value shift) generates an event of the given value
shift-seconds before the start time.

:final (cons value shift) generates an event of the given value
shift-seconds after the end time.")
