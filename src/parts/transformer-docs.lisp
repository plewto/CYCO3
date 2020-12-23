;;;; CYCO part transformer-docs.lisp
;;;;

(in-package :cyco-part)

(constant +transformer-docstring+
	  "TRANSFORMER is a part for creating transformations on other
parts.   It consist of two components.

1) A filter to indicate which events are to be modified.
2) A transform function for creating the new events.


name         - symbol
source       - Part or midi-event list.  The events to be modified.
:filter      - Function to discriminate effected parts.  See below.
:transform   - Function to create altered version of selected parts.  See below.
:render-once - Boolean, if true do not repeat.
:remarks     - Optional remarks text.


The filter function has form (lambda part event) -> bool
part is an instance of transformer and 
event is a cons (time . midi-message).
The filter should return true for events to be processed.


The transform function has the form  (lambda part event) -> event(s)
and takes the same arguments as the filter-function and returns 
a list of events.")




