;;;; CYCO
;;;;
;;;; Defines all generic CYCO functions.
;;;; In a few cases implements simpler methods.
;;;;

(defmacro def-type-predicate (name &optional documentation)
  "Defines a new generic predicate function. 
Implements the default, false, method.
A true method must be implemented manually."
  `(progn
     (defgeneric ,name (object)
       (:documentation (or ,documentation "Predicate")))
     (defmethod ,name ((object t)) nil)))

(defgeneric ->cycle (object)
  (:documentation
   "Coerce object to Cycle pattern.
If object is an instance of Cycle, return it.
If object is some non-cycle Pattern, return a new Cycle with the elements of object.
If object is a list, return new Cycle :of object.
For all other types, coerce object to a list and return new Cycle."))

(defgeneric ->list (object)
  (:documentation
   "Coerce object to a list.
If object is a list, return it.
If object is some other sequence type, return new list of object's elements.
For all other types, create new list with object as it's only element.
see ->vector"))

(defgeneric ->markov-link (object)
  (:documentation "Coerce object to a markov-link"))

(defgeneric ->pattern (object &key pattern-type)
  (:documentation
   "Coerce object to Pattern of type pattern-type.
If object is a pattern-type, return it.
If object is a non pattern-type Pattern, return new instance of pattern-type with elements taken
from object.
For all other types return (pattern-type :of object)
The default pattern-type is Cycle."))


(defgeneric ->midi (contents &key filename offset repeat pad)
  (:documentation
   "Creates new MIDI file from contents and write to file.
:filename - Optional String, if not specified the filename is derived from
            the name of contents and the current project.
:offset   - float, initial offset time in seconds, default 0.
:repeat   - int, number of times to repeat contents, default 1.  If repeat is too 
stack depth may be exhausted.           
:pad      - float, number of seconds to add to end of file, default 2."))

(defgeneric ->string (object)
  (:documentation
   "Returns string representation of object."))

(defmethod ->string ((object t))(format nil "~A" object))
(defmethod ->string ((s string)) s)
(defmethod ->string ((lst list)) (mapcar #'->string lst))

(defgeneric ->symbol (arg &optional package)
  (:documentation
   "Intern new symbol using name of argument."))

(defmethod ->symbol ((s symbol) &optional (package :cyco))
  (if (eq (symbol-package s)(find-package package))
      s
    (intern (string-downcase s) package)))

(defmethod ->symbol ((symbol-name string) &optional (package :cyco))
  (intern (string-upcase symbol-name) package))

(defmethod ->symbol ((n number) &optional (package :cyco))
  (->symbol (->string n) package))

(defgeneric ->vector (object)
  (:documentation
   "Coerce object to vector.
If object is a vector, return it.
If object is a non-vector sequence, return new vector using elements of object.
For all other cases return new vector with object as it's only element.
See ->list"))

(defgeneric ? (object)
  (:documentation
   "Generalized help/inspection function."))

(defmethod ? ((object t))
  (format t "~A~%" (type-of object)))

(defmethod ? ((object symbol))
  (if (boundp object)
      (format t "Symbol ~A --> value ~A~%" object (symbol-value object)))
  (if (fboundp object)
      (describe object)))

(defgeneric add-group (section group)
  (:documentation
   "Adds group to section."))

(defgeneric articulation-map (object)
  (:documentation
   "Returns objects's articulation-map."))

(defgeneric articulation-map! (object mapfn)
  (:documentation
   "Sets objects's articulation-map"))

(def-type-predicate bag-p
   "Predicate, true if argument is an instance of Bag pattern.")

(defgeneric bar-duration (time-signature)
  (:documentation
   "Returns duration of single bar from time-signature."))

(defgeneric bars (time-signature)
  (:documentation
   "Returns time-signature bar count."))

(defgeneric bars! (time-signature value)
  (:documentation
   "Sets time-signature bar count."))

(defgeneric beat-duration (time-signature)
  (:documentation
   "Returns time-signature beat duration."))

(defgeneric beats (time-signature)
  (:documentation
   "Returns time-signature beat count."))

(defgeneric beats! (time-signature value)
  (:documentation
   "Sets time-signature beat count."))

(defgeneric butfinal (object)
  (:documentation
   "Returns all but final element of sequence.
(butfinal '(a b c)) --> (a b)"))

(defgeneric cardinality (pattern)
  (:documentation
   "Returns number of elements in a Pattern."))

(defgeneric channel (object &optional resolve)
  (:documentation
   "Returns MIDI channel of object.
MIDI channels may be symbolic and one symbolic channel may map 
to another symbol.  I.E. channel 'snare may map to channel 'drum.
If the optional resolve argument is true the result is the actual 
MIDI channel, an integer between 1 and 16 inclusive."))

(defmethod channel ((object null) &optional resolve)
  (dismiss resolve)
  1)

(defmethod channel ((n integer) &optional resolve)
  (dismiss resolve)
  n)

(defgeneric channel! (instrument channel)
  (:documentation
   "Sets instrument's MIDI channel.  
channel may be a symbolic meta-channel or an integer between 1 and 16 
inclusive."))

(defgeneric channel-index (object)
  (:documentation
   "Returns MIDI channel-index of object.
The channel-index is the lower 4-bits of a MIDI channel-message status byte,
and is always in the range 0..15 inclusive.  The channel-index is 1 less then 
the MIDI channel.   User facing functions should generally use the actual MIDI 
channel instead of the channel index."))

(defmethod channel-index ((n null)) 0)
(defmethod channel-index ((channel integer))(logand (1- channel) #x7F))

(defgeneric child-of-p (parent child)
  (:documentation
   "Predicate, true if child is a direct child of parent.  See cyco-node."))

(def-type-predicate chord-model-p
   "Predicate, true if object is an instance of chord-model.")


(defgeneric chord-template (chord-model chord-type keynumber)
  (:documentation
   "Returns chord as list.
If the chord-model is absolute the result is list of key-numbers.
Other the result is a list of key-number offsets.

chord-model - The chord model
chord-type - symbol
keynumber - For some chord-models the chord 'shape' may change for different key-numbers.
Other chord-models may ignore this value."))


(defgeneric chord-types (chord-model)
  (:documentation
   "Returns list of chord types defined by chord-model."))

(defgeneric clone (source &key new-name new-parent)
  (:documentation
   "Creates copy of source.
Clone is defined for all types. However, for types where a specific clone
method has not been defined, it returns the source argument.


:new-name   - Format string, inclusion of the format ~A directive inserts 
              the source name into the result.  For object types which do not
              have a name, new-name is ignored.
:new-parent - By default the cloned object has the same parent as the source,
              new-parent explicitly sets the parent of the result.  For objects
              which do not have a parent/child relationship, new-parent is 
              ignored."))

(defmethod clone ((object t) &key new-name new-parent)
  (dismiss new-name new-parent)
  object)

(defmethod clone ((src hash-table) &key new-name new-parent)
  (dismiss new-name new-parent)
  (let ((dst (make-hash-table :size (hash-table-count src))))
    (maphash #'(lambda (k v)(setf (gethash k dst)(clone v))) src)
    src))

(defmethod clone ((c cons) &key new-name new-parent)
  (dismiss new-name new-parent)
  (cons (clone (car c))(clone (cdr c))))

(defmethod clone ((v vector) &key new-name new-parent)
  (dismiss new-name new-parent)
  (->vector (clone (->list v))))

(defgeneric cnth (n seq)
  (:documentation
   "Returns nth element of sequence in a circular manner.
(cnth 2 '(a b c)) --> C
(cnth 3 '(a b c)) --> A
(cnth 4 '(a b c)) --> B"))

(defgeneric connect (parent child)
  (:documentation
   "Make parent node the parent of child.
If child has an existing parent, that connection is severed."))

(def-type-predicate cycle-p
   "Predicate, true if object is an instance of Cycle.")

(defgeneric data (object index)
  (:documentation
   "Returns the indexed value from object."))

(defgeneric data-count (object)
  (:documentation
   "Returns number of data values in object."))

(defgeneric define-chord (chord-model name template &optional description)
  (:documentation
   "Defines a new chord.
chord-model - An instance of chord-model.   
name - Symbol
template - List of key-number offsets.
description - Optional text."))

(defgeneric defines-chord-p (chord-model chord-name)
  (:documentation 
   "Predicate, true if chord-model defines the named chord."))

(def-type-predicate dice-p
   "Predicate, true if object is an instance of Dice.")

(defgeneric disconnect (child)
  (:documentation
   "Severs connection between child and it's parent, if any. 
After a disconnect the child becomes a root node."))

(defgeneric dump-chords (chord-model)
  (:documentation
   "Displays list of chords defined by chord-model."))

(defmethod dump-chords ((object null)) )

(defgeneric dump-events (object &key range filter render)
  (:documentation
   "Display listing of MIDI events.
obj - Source of event
:range - time range (start end)
:filter - function, predicate on events.  If the result it true do not 
display the event.
;render - If true display actual MIDI bytes."))

(defgeneric dump-smf-track-chunk (track pad)
  (:documentation
   "Diagnostic displays contents of MIDI file track."))

(defgeneric duration (time-signature)
  (:documentation
   "Returns duration of time-signature phrase."))

(defmethod duration ((n number))(float n))

(defgeneric dynamic (dynamic-value)
  (:documentation
   "Returns dynamic value of object or list of objects."))

(defgeneric dynamic->velocity (dynamic-value)
  (:documentation
   "Convert dynamic value to MIDI velocity."))

(defgeneric dynamic-map (instrument)
  (:documentation
   "Returns instrument's dynamic-map function."))

(defgeneric dynamic-map! (instrument mapfn)
  (:documentation
   "Sets instrument's dynamic-map function."))

(defgeneric dynamic-name (dynamic-value)
  (:documentation
   "Returns symbolic name of dynamic value.
Note this function is generally inefficient and there may be slight 
rounding errors."))

(def-type-predicate dynamic-p
  "Predicate, true if argument can be interpreted as a dynamic value.")

(defgeneric final (sequence)
  (:documentation
   "Returns final element from sequence."))

(defgeneric find-child (parent child)
  (:documentation
   "Find indicated child of parent.
Returns nil if no such child exists."))

(def-type-predicate group-p
   "Predicate, true if object is an instance of Group.")

(defgeneric hash-property (node key)
  (:documentation
   "Predicate, true if node has indicated property."))

(defgeneric has-group-p (section group-name)
  (:documentation 
   "Predicate, true if section contains named group."))

(defgeneric init-time-signature (time-signature)
  (:documentation
   "Initialize time-signature."))

(def-type-predicate instrument-layer-p
   "Predicate, true if object is an instance of Instrument-layer.")

(def-type-predicate instrument-p
   "Predicate, true if object is an instance of Instrument.")

(defgeneric invert (key-number pivot-key)
  (:documentation
   "Apply key number inversion to key-number.
Inversion may be applied to any object without producing an error.
If the object has the property :transposable with a nil value, then no 
inversion is applied. 

The pivot-key is a key-number around which the inversion takes place.
All negative values have an inversion of +REST+ = -1.

Ignore transformation if pivot is nil.

(invert '(-1234 59 60 70) 60) --> (-1 61 60 50)"))

(defmethod invert ((object t) pivot) object)

(defgeneric keyname (key-number)
  (:documentation
   "Returns symbolic name for key-number.
For equivalent enharmonic notes, the sharp value is always used.
Keyname will not return names for user defined keynumbers."))


(defgeneric keynumber (object)
  (:documentation
   "Converts object, or list of objects, to MIDI key-numbers."))

(defgeneric keynumber-map (instrument)
  (:documentation
   "Returns instrument's keynumber-map function."))

(defgeneric keynumber-map! (instrument keynumber-map)
  (:documentation
   "Sets instrument's keynumber-map function."))

(def-type-predicate keynumber-p
   "Predicate, true if object can be interpreted as a key-number.")

(def-type-predicate line-p
   "Predicate, true if object is an instance of Line.")

(defgeneric local-properties (node)
  (:documentation
   "Returns list of object's local-property names.  
A local property is a property defined directly by an object.
Properties defined by an object's parent are not considered local."))

(def-type-predicate markov-chain-p)
(def-type-predicate markov-link-p)

(defgeneric markov-add-link (source destination weight)
  (:documentation
   "Adds potentail destinations for a Markov-link.
source - the markov-link
destination - potentail next value.  The destination may be of any type
but is coerced to a markov-link.
weight - The number of times destination is added to sources' links list."))

(defgeneric markov-walk (markov-link)
  (:documentation
   "Returns the next markov-link."))

(defgeneric meta-channel (channel-name  &optional resolve)
  (:documentation
   "Returns value of named MIDI channel.  A symbolic meta-channel may
map to another symbolic channel.  If optional resolve argument is true
then the actual MIDI channel, as an integer between 1 and 16 inclusive, 
is returned."))

(defgeneric metric (metric-value)
  (:documentation
   "Converts object, or list of objects, to float metric values."))

(defgeneric metric-expression (metric-expression)
  (:documentation
   "Evaluates metric-expression and return it's value as a float."))

(defgeneric metric-expression-p (metric-expression)
  (:documentation
   "Returns non-nil if exp may be interpreted as a metric-expression.
Parsing metric-expressions may be slow and exp is evaluated to determine
if it is a valid expression.  If exp is invalid the result is nil.  If
exp is a valid metric-expression, the result is its float value."))

(def-type-predicate metric-p
   "Predicate, true if argument may be interpreted as a basic metric value.")

(def-type-predicate midi-channel-message-p
   "Predicate, true if object is a MIDI channel message.")

(def-type-predicate midi-channel-pressure-p
   "Predicate, true if object is a MIDI channel pressure message.")

(def-type-predicate midi-control-chnage-p
   "Predicate, true if object is a MIDI control change message.")

(def-type-predicate midi-end-of-track-p
   "Predicate, true if object is a MIDI end of track message.")

(def-type-predicate midi-end-system-exclusive-p
   "Predicate, true if object is a MIDI end of system-exclusive message.")

(def-type-predicate midi-key-message-p
   "Predicate, true if object is a keyed MIDI message.  Keyed messages 
include: note-off, note-on and poly-pressure.")

(def-type-predicate midi-key-signature-p
   "Predicate, true if object is a MIDI key-signature message.")

(def-type-predicate midi-message-p
   "Predicate, true if object is any type of MIDI message.")

(def-type-predicate midi-meta-copyright-p
"Predicate, true if object is a midi-meta-copyright message.")

(def-type-predicate midi-meta-cue-p
"Predicate, true if object is a midi-meta-cue message.")

(def-type-predicate midi-meta-instrument-name-p
"Predicate, true if object is a midi-meta-instrument-name message.")

(def-type-predicate midi-meta-lyric-p
"Predicate, true if object is a midi-meta-lyric message.")

(def-type-predicate midi-meta-marker-p
"Predicate, true if object is a midi-meta-marker message.")

(def-type-predicate midi-meta-message-p
"Predicate, true if object is any type of MIDI meta message.")

(def-type-predicate midi-meta-text-p
"Predicate, true if object is any type of MIDI meta text message.")

(def-type-predicate midi-meta-track-name-p
"Predicate, true if object is a midi-meta-track-name message.")

(def-type-predicate midi-note-off-p
"Predicate, true if object is a midi-note-off message.")

(def-type-predicate midi-note-on-p
"Predicate, true if object is a midi-note-on message.")

(def-type-predicate midi-pitch-bend-p
"Predicate, true if object is a midi-pitch-bend message.")

(def-type-predicate midi-poly-pressure-p
"Predicate, true if object is a midi-poly-pressure message.")

(def-type-predicate midi-program-change-p
"Predicate, true if object is a midi-program-change message.")

(def-type-predicate midi-system-common-message-p
"Predicate, true if object is a midi-system-common-message message.")

(def-type-predicate midi-system-exclusive-p
"Predicate, true if object is a midi-system-exclusive message.")

(def-type-predicate midi-tempo-message-p
"Predicate, true if object is a midi-tempo-message message.")

(def-type-predicate midi-time-signature-p
"Predicate, true if object is a midi-time-signature message.")

(defgeneric mnemonic (object)
  (:documentation
   "Returns short mnemonic string for object 
Mnemonic is primarily used for MIDI message type abbreviations."))

(defmethod mnemonic ((object t))
  (->string object))

(defgeneric mute (object &optional state)
  (:documentation
   "Sets mute state of object.
Possible states are :mute :unmute :solo and nil
For solo this object is switched to non-muted while all sibling objects
are muted.  A nil value leaves the current state as is."))

(defgeneric mute-all (object)
  (:documentation
   "Sets all child objects to muted."))

(defgeneric muted-p (object)
  (:documentation 
   "Predicate, true if object is muted."))

(defgeneric name (object)
  (:documentation
   "Returns object's name.  Name may be applied to any object without error."))

(defmethod name ((object t)) nil)
(defmethod name ((s symbol)) (symbol-name s))
(defmethod name ((s string)) s)

(defgeneric name! (object new-name)
  (:documentation
   "Sets name of object."))

(defgeneric next (pattern &optional n)
  (:documentation
   "Returns next value(s) from Pattern or pattern-like object.
By default a single value is returned (see next-1).
If n is a positive integer, returns list of the next n values.
If n is the keyword :rest, returns list of all remaining values
If n is the keyword :all, returns list of all elements.
See next-1 and next-n."))

(defgeneric next-1 (pattern)
  (:documentation
   "Returns the next value from a Pattern or pattern-like object."))

(defgeneric next-n (pattern n)
  (:documentation
   "Returns a list of the next n values from a Pattern or pattern-like 
object."))

(defgeneric note-events (instrument time key-number duration dynamic &key time-scale)
  (:documentation
   "note-events is a low-level method to convert time, key, duration and 
dynamics to MIDI events using a specific instrument.  The instrument's 
keynumber, articulation and dynamic maps are applied to the result.
The result is a two-element list 
    ((time . note-on)
     (time+duration . note-off))"))

(defgeneric octave (key-number)
  (:documentation
   "Returns octave number of key number or list of key numbers.
All negative key numbers have octave -1."))

(defgeneric palindrome (object &key elide)
  (:documentation
   "Creates palindrome from source sequence.
The :elide argument determine how end-values are treated.
(palindrome '(A B C D))  --> (A B C D D C B A)
(palindrome '(A B C D) :elide :last)  --> (A B C D C B A)
(palindrome '(A B C D) :elide :first) --> (A B C D D C B)
(palindrome '(A B C D) :elide :both)  --> (A B C D C B)"))

(defmethod palindrome (object &key elide)
  (dismiss elide)
  (clone object))

(defgeneric parent (node)
  (:documentation
   "Returns object's parent.   If object is a root node, returns nil."))

(def-type-predicate part-p
   "Predicate, true if object is an instance of any type of Part.")


(defgeneric path-to-root (node)
  (:documentation
   "Returns list of all nodes between this object and the root node (inclusive)."))

(def-type-predicate pattern-p
   "Predicate, true if object is any type of Pattern.")

(defgeneric permute (object)
  (:documentation
   "Returns permutation of sequence."))

(defmethod permute ((object t)) object)

(defgeneric phrase-duration (time-signature)
  (:documentation
   "Returns time-signature phrase-duration."))

(defgeneric pick (object)
  (:documentation
   "Select an object at random from collection."))

(defmethod pick ((object t)) object)
(defmethod pick ((n integer)) (random n))

(defgeneric pitch-class (key-number)
  (:documentation
   "Returns the pitch-class, an integer between 0 and 11 inclusive, 
of a key-number or list of key numbers.  All negative key-numbers
have a pitch class of -1."))

(defgeneric print-tree (node &optional depth)
  (:documentation
   "Print hierarchal tree starting at node.  
The depth argument is used internally."))

(defgeneric priority (object)
  (:documentation
   "Returns the priority of a MIDI message.  If two MIDI events have the 
same time stamp, the event-message with the lowest priority appears first in 
the events list.  There are primarily two applications.  First it ensures 
certain meta events appear in the correct location.  I.E. end-of-track always
appears after all other events.  Secondly, note-off events always appear after 
note on-events to prevent stuck notes."))

(defgeneric program-bank (instrument)
  (:documentation
   "Returns an instruments program-bank.
There is no pre-defined format for a program-bank, it is up to the specific
instrument.   

NOTE: This is not a 'bank' of programs, rather it is the name of a bank on some 
external MIDI device."))

(defgeneric program-bank! (instrument program-bank)
  (:documentation
   "Sets instruments program bank."))

(defgeneric program-change-events (instrument time &key bank program)
  (:documentation
   "Generates list of MIDI events to effect an instrument's program change.
The result format is dependent on the specific external MIDI instrument.
At it's simplest the result is a single program-change event.  
More complex results may include control changes for bank selection.  
The result may also include note events for 'key-switched' instruments."))

(defgeneric program-map (instrument)
  (:documentation
   "Returns an instrument's program-map function."))

(defgeneric program-map! (instrument program-function)
  (:documentation
   "Sets instrument's program-map function."))

(defgeneric program-number (instrument)
  (:documentation
   "Returns instrument's default program-number.  There is no predefined 
format, though integers between 0 and 127 are generally treated as MIDI 
program numbers.  The exact format is dictated by the program-map 
function."))

(defgeneric program-number! (instrument program-number)
  (:documentation
   "Returns instrument's program-number."))

(def-type-predicate project-p
   "Predicate, true if object is an instance of Project.")

(defmethod project-p ((object null)) nil)

(defgeneric properties (node &optional acc)
  (:documentation
   "Returns list of all property keys defined by object."))

(defgeneric property (node key)
  (:documentation
   "Returns object's property value assigned to key.
It is an error if the object does not define key as a property."))

(defgeneric prune (node &optional force)
  (:documentation
   "Starting at node recursively disconnect all child nodes for which the 
:TRANSIENT property is true.  If force is true, disconnect all nodes,
ignoring their transient value."))

(defgeneric push-event (time midi-message object)
  (:documentation
   "Push a new MIDI event to object. 
time - float
message - MIDI message
object - destination."))

(defgeneric put (node key value)
  (:documentation
   "Assign's value to an object,s property.
object - the object
key - symbol, property name.  It is an error if the object does not 
define key as a property.
value - the new value."))

(defgeneric remaining (pattern)
  (:documentation
   "Returns list of the remaining values yet to be returned by a Pattern
or pattern-like object."))

(defgeneric remarks (node)
  (:documentation
   "Returns object's remarks text."))

(defgeneric remarks! (node text)
  (:documentation
   "Sets object's remarks text."))

(defgeneric render-midi-message (midi-message)
  (:documentation
   "Convert MIDI message object to list of MIDI bytes."))

(defgeneric render-n (object count &key offset)
  (:documentation
   "Convert object to list of MIDI events.
If count is greater then 1, place each additional copy shifted by the 
duration of the object.  

:offsets shifts all events by offset seconds, default 0.

Stack space may be exhausted if count is too high."))

(defgeneric render-once (object &key offset)
  (:documentation
   "Convert object to a list of MIDI events.
:offset shifts all events by offset seconds, default 0."))

(defgeneric render-smf (midi-file &key pad)
  (:documentation
   "Converts Standard MIDI file object to list of MIDI events.
:pad adds pad seconds at end of events."))

(defgeneric render-smf-header (object track-count)
  (:documentation
   "Low-level method, renders MIDI file header."))

(defgeneric render-smf-track (track pad)
  (:documentation
   "Low-level method, renders MIDI file track."))

(defgeneric reset (object)
  (:documentation
   "Restore internal state of object to some initial condition.
See also soft-reset."))
   
(defmethod reset ((object t)) object)

(defgeneric rest-p (object)
  (:documentation 
   "Predicate, true if object is interpreted as a rest.
Generally any negative number, and the symbol 'r are treated as rest."))

(defgeneric retrograde (object)
  (:documentation
   "Reverse object's elements. 
If the object has the :RESERVABLE property with a value of nil, do not 
apply.   Retrograde may be called with any object without producing an
error."))

(defmethod retrograde ((object t)) object)
(defmethod retrograde ((seq sequence))(reverse seq))

(defgeneric root-p (node)
  (:documentation 
   "Predicate, true if object is a root node."))

(def-type-predicate section-p
   "Predicate, true if object is an instance of Section or one of it's
subclasses.")

(defgeneric section-order (sections &key project)
  (:documentation
   "Sets the sequence order of Sections within a project."))

(def-type-predicate slew-p)

(defgeneric slice (sequence start &optional end)
  (:documentation
   "Returns a slice of a sequence.
seq - 
start - Staring index 
end   - Ending index, may be negative to index from end, defaults to end
of sequence."))

(defgeneric smf-track (midi-file &optional track-number)
  (:documentation
   "Low-level method, returns indexed track from MIDI file.
NOTE: At least for now CYCO only supports single-track MIDI files."))

(defgeneric smf-track! (midi-file track &optional track-number)
  (:documentation
   "Low-level method to set MIDI file track.
NOTE: At least for now CYCO only supports single-track MIDI files."))

(defgeneric smf-track-count (midi-file)
  (:documentation
   "Returns number of tracks in MIDI file."))

(defgeneric soft-reset (object)
  (:documentation
   "Performs a soft-reset on object.   A soft reset initializes some of an 
objects parameters but not all of them.  This is in contrast to a normal 
reset which initializes all values.  The exact behavior is dependent on the 
specific object type."))

(defmethod soft-reset ((object t)) object)

(defgeneric solo (object)
  (:documentation
   "Convenience method same as (mute object :solo)"))

(defgeneric subbeat-duration (time-signature)
  (:documentation
   "Returns time-signature sub-beat duration."))

(defgeneric subbeats (time-signature)
  (:documentation
   "Returns time-signature sub-beat count."))

(defgeneric subbeats! (time-signature value)
  (:documentation
   "Sets time-signature sub-beat count."))

(defgeneric tbeats (time-signature)
  (:documentation
   "Returns time-signature triplet-beat count"))

(defgeneric tbeat-duration (time-signature)
  (:documentation
   "Returns time-signature triplet-beat duration."))

(defgeneric tempo (time-signature)
  (:documentation
   "Returns time-signature tempo in BPM."))

(defgeneric tempo! (time-signature tempo-bpm)
  (:documentation
   "Sets time signature tempo in BPM."))

(defgeneric tick-duration (time-signature &key unit)
  (:documentation
   "Returns duration of a single tick for time-signature."))

(defgeneric ticks-per-beat (time-signature)
  (:documentation
   "Returns the time-signature ticks per beat count."))

(defgeneric tsubbeats (time-signature)
  (:documentation
   "Returns time-signature subbeat triplet count."))

(defgeneric tsubbeat-duration (time-signature)
  (:documentation
   "Returns time-signature subbeat triplet duration."))

(defgeneric transpose (object amount)
  (:documentation
   "Apply key-number transposition on object.
Transpose may be called on any object type without producing an error.
If an object has :TRANSPOSABLE value set to nil, it is not transposed.
object - The object or list of objects.
amount - integer, transposition amount."))

(defmethod transpose ((object t) amount) object)

(defgeneric unit (time-signature)
  (:documentation
   "Returns the time-signature beat unit."))

(defgeneric unit! (time-signature beat-unit)
  (:documentation
   "Sets time-signature beat-unit."))

(defgeneric unmute (object)
  (:documentation
   "Convenance method, same as (mute object :unmute)"))

(defgeneric unmute-all (object)
  (:documentation
   "Unmute all child elements of object."))

(defgeneric value (object))
(defmethod value ((object t)) object)

(def-type-predicate walker-p)
(def-type-predicate wrapper-p)

(defgeneric write-smf (object filename &key pad no-overwrite)
  (:documentation
   "Write MIDI file object to disc."))


