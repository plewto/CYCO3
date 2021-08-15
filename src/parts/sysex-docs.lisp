;;;; CYCO parts sysex-docs.lisp
;;;;
;;;; Docstring for SYSEX part.

(in-package :cyco-part)

(constant +make-sysex-docstring+
	  "Creates new instance of SYSEX part.

Most keyword arguments default to parent section value.

name - symbol, part name
:section - parent section, defaults to current section of current project.
:cuefn - cue function, defaults to parent section value.
:shuffle - shuffle function, defaults to parent section value.
:shift - Fixed offset in seconds
:render-once - Boolean, if non-nil do not repeat when rendering.
:remarks - Optional remarks text
:events * See below

Sysex events have the form of a nested list:

     ((time-1 values ...)
      (time-2 values ...))

Where time-n must be in format expected by the cue and shuffle functions, 
and values are the message data.  Each value 0 <= value < 128.
Do not include either the SYSEX or END-OF-SYSEX status bytes.")


(constant +sysex-docstring+
	  "SYSEX macro is nearly identical to the MAKE-SYSEX function.
The name argument should be quoted for MAKE-SYSEX and unquoted for SYSEX.
The SYSEX macro binds the new part to the symbol name.")
