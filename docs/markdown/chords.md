# CYCO Chords

Chords are defined in one of two ways:

1. Relative, a list of keynumber-offsets, IE (0 4 7) for a major triad.
2. Absolute, a list of keynumbers, IE (60 67 67) for a C-major chord.

The global structure **\*CHORD-TABLE\*** defines about 100 chord-types
accessible by name.  \*CHORD-TABLE\* takes a keyboardist view, several
alternate tables, available via plugins, define chords as played on
fretted instruments.  These include:

- oud, a framework for defining chords on fretted instruments.
- bass-chord, bass guitar chords.
- guitar-chords
- mandolin-chords
- ukulele-chords

\*CHORD-TABLE\* and the structures defined by the Oud plugins are instances
of the **CHORD-MODEL** class. In use there is no difference between the
global \*CHORD-TABLE\* and the tables produced by the Oud plugins.  The
available chord types will be different but they are used in exactly the
same way.


**(DEFCHORD name template &optional description)**

    Add chord to the global *CHORD-TABLE*
	
	name     - An unofficial style is to name chords inside square brackets: 
	           [major], though this is not universally followed. 
	template - list of keynumber offsets.
	
**(?CHORDS &optional (model *chord-table*))**

    Display list of defined chords.
	
**(CHORD-INVERSION template degree &key add-octave)**

    Returns inverted form of chord.
	
	template    - List of keynumbers or keynumber offsets.
	degree      - Sets number and direction of rotations applied to template.
	              Positive values rotate the template to the right with the 
				  left-most value being transpose up an octave.
				  
	              For a template (0 4 7)
				  
				  degree 0 --> (0 4 7)   no change
				  degree 1 --> (4 7 12)
				  degree 2 --> (7 12 16)
				  
				  Negative values rotate the template to the left, the
				  right most value is transposed down an octave.
				  
				  degree -1 --> (-5 7 12)
				  degree -2 --> (-8 -5 7)
				  
				  Note the negative values are treated as transpositions of
				  the root note, they do not represent rest.
				  
	:add-octave - integer, if non-zero adds a transposed copy of the
                  left-most note to the end of template -after- rotation
				  has been applied.  The value indicates how many octaves
				  to transpose. 
				  
				  (chord-inversion '(0 4 7) 1 :add-octave 0)  --> (4 7 12)
                  (chord-inversion '(0 4 7) 1 :add-octave 1)  --> (4 7 12 16)
				  (chord-inversion '(0 4 7) 1 :add-octave -1) --> (4 7 12 -8)
