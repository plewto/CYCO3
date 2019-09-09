# CYCO Sections

The **SECTION** class extends **TIME-SIGNATURE** and represents a major
composition division. <br>

Sections are the middle layer, they are always a child of a project and
only have parts as child nodes. <br>

The primary job of a section is to be a container for it's parts.  Sections
often override the project time-signature and may mute or solo individual
parts or groups of parts. <br><br>

**(MAKE-SECTION name &key tempo unit bars beats subbeats cuefn shuffle transposable
reversible remarks project)** <br>

**(SECTION name &key tempo unit bars beats subbeats cuefn shuffle transposable
reversible reamers project)** <br>

    Creates new SECTION object.
	
	SECTION and MAKE-SECTION are identical except that SECTION binds the
	new instance to the symbol name, while MAKE-SECTION does not.  The
	name argument should be quoted for MAKE-SECTION and unquoted for
	SECTION.
	
	
    :tempo        - float, tempo in BPM, defaults to project value.
    :unit         - symbol, time-signature beat-unit, defaults to project value.
    :bars         - int, length of section in bars, defaults to project value.
    :beats        - int, number of beats per bar, defaults to project value.
    :subbeats     - int, number of subbeats per beat, defaults to project value.
    :cuefn        - function, defaults to project value.
	:shuffle      - function, defaults to project value.
    :transposable - Boolean, if nil the section is not effected by transpose
                    and key-inversion operations. May be overridden by individual
                    parts.
    :reversible   - Boolean, if true the section is not effected by retrograde 
                    operations. May be overridden by individual parts.
    :remarks      - Optional remarks text solely for the users use.
    :project      - The parent project, defaults to *PROJECT*


    See cue and shuffle functions.

By default the new Section is designated the *current* section of it's
parent project.   Most section related functions default to the current
section of \*PROJECT\*.


**(GROUP name members)**

    Creates new group in the current section.  
	
	Groups combine related sets of parts for mute and solo operations.
	The group object is bound to the symbol name.
	
	name    - Unquoted symbol. 
	members - List of member part names.  All parts must currently exist
	          within the section.
			  
	Example:
	(group percussion '(kick snare toms))
	

**(MUTE-ALL section)**  <br>
**(UNMUTE-ALL section)**

    mute/unmute all parts in section.
	
**(MUTE group-or-part &optional state)**

    Change mute status of a group or part.
	
	Possible states are:
	
	:mute   - mute part or all members of a group. 
	:unmute - unmute part or all members of a part.
	:solo   - unmute this group or part, mute all sibling groups and parts.
	nil     - maintain current mute status.
	
**(CLONE section &key new-name new-parent)**

    Creates clone of section and all of it's member parts.
	
	:new-name   - Specifies format string for renaming the clone.
	              The default format is "~A" where ~A is replaced by the
				  source section's name.
	:new-parent - Sets the parent of the clone.  By default the clone has
	              the same parent as the original.
				  
**(TRANSPOSE section n)**   <br>
**(INVERT section pivot)**  <br>
**(RETROGRADE section)**

    Applies indicated operation to all member parts.
	
	Transpose and invert are applied only if the section/part TRANSPOSABLE
	property is true.
	
	Retrograde is applied only if the section/part REVERSIBLE property is true.
	
**(RENDER-ONCE section &key (offset 0))**

    Generates MIDI event-list from section.
	
	Adds offset seconds to event times.
	
**(RENDER-N section count &key (offset 0.0))**

    Generates MIDI event-list from section.
	
	count - positive integer, number of time to repeat section.
	offset - time, add offset seconds to all event times.

**(->MIDI section &key filename repeat pad)**

    Writes MIDI file from section events.
	
	:filename - output filename, defaults projects MIDI directory with the
	            same name as section.    For verse section in project foo,
	            the default name is
				
				~/cyco-projects/foo/MIDI/verse.mid
				
	:repeat   - Number of repeats, default 1
	:pad      - Time in seconds added at the end to allow for final decay
	            tails. Default 0.0
	

-----

## Anatomy of a Section File

    ;; ~/cyco-projects/foo/verse.lisp
    
    
    ;; Create section object and bind to symbol VERSE
    ;;     The section becomes the 'current' section in *PROJECT*
    ;;     The CYCO prompt is updated to indicate the current section.
    ;;
    
    (section verse :bars 4) 
    
    ;; Optional metronome part.
    ;; 
    
    (metronome)
    
    ;; Create parts here...
    ;;    For illustration assume the parts verse-kick,
    ;;    verse-snare, verse-bass and verse-piano have
    ;;    been created.
    
    ;; Create part groups.  Groups are used for the purpose of 
    ;;     muting or soloing collections of parts.  These group 
    ;;     objects are bound to PERCUSSION and INSTRUMENTS respectively.
    
    (group percussion '(verse-kick verse-snare))
    (group instruments '(verse-bass verse-piano))
    
    ;; Set mute state of groups and individual parts.
    ;;    Possible states are :mute :unmute :solo and nil
    ;;    A nil value does not alter the current status.
    ;;
    
    (mute percussion  :unmute)
    (mute instruments :unmute)
    (mute metronome   :mute)
    (mute verse-kick  nil)
    (mute verse-snare nil)
    (mute verse-bass  nil)
    (mute verse-piano nil)
    
    ;; Create two section MIDI files.
    ;;   The files are saved as: 
    ;;      ~/cyco-projects/foo/MIDI/verse.mid
    ;;      ~/.cyco-projects/foo/MIDI/test-verse.mid
    ;;
    
    (->midi verse)
    (->midi verse :repeat 8 :filename "test-filename")
	
