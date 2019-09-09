# CYCO Projects

A *Project* is the top level composition object and is composed of one or
more sequential *sections*. Each section is composed of one or more
parallel *parts*.

- Project - represents the piece as a whole.
- Section - a major division, i.e. verse, chorus, etc...
- Part - individual instrument parts within a section.

The PROJECT, SECTION and PART classes are all extensions of TIME-SIGNATURE,
which is itself an extension of CYCO-NODE.   This allows the project to
define composition-level defaults.  Each section and part inherit these
values but are free to override them.

## Project File Structure

Each project has it's own directory in *~/cyco-projects/*  The project
directory has the same name as the project converted to lower case.  For a
project named 'Foo' the directory structure is:

    ~/cyco-projects/
	   |
	   +-- foo/
	        |
			+-- foo-main.lisp 
			+-- additional-project-files
			+-- 
			+-- MIDI/
			     |
				 +-- foo.mid
				 +-- additional-MIDI-files
				 +--
				 
The following global variables set the location and names for project
files.

- **\*DEFAULT-PROJECT-DIRECTORY\***
- **\*PROJECT-MAIN-FILENAME-FORMAT\***
- **\*DEFAULT-PROJECT-OUTPUT-DIRECTORY\***

## Creating and Loading Project Files


**(?PROJECTS)**

    Displays list of directories under ~/cyco-projects/


**(CREATE-PROJECT-FRAMEWORK files)**

    Creates an empty project directory 
	
	The files argument is either a single project name (as symbol) or a
    list of the name followed by additional file names.
	
	An empty file is created for each specified filename.   If a project 
	by the same name already exists, the function prints a warning and
	returns without creating anything.
	
	(create-project-framework 'foo)
	
	     ~/cyco-projects/
		    |
			+-- foo/
			     |
				 +-- foo-main.lisp
				 +-- MIDI/
				 
	(create-project-framework '(foo alpha beta))
	
		~/cyco-projects/
		    |
			+-- foo/
			     |
				 +-- foo-main.lisp
				 +-- alpha.lisp
				 +-- beta.lisp
				 +-- MIDI/
				 
	
**(LOAD-PROJECT name)**

    Loads project's main file
	
	It is the main files responsibility to load the remaining project
    files.
	
	(LOAD-PROJECT 'foo)

	   loads file ~/cyco-projects/foo/foo-main.lisp
	   
	After loading the project object is bound to the global variable
	*PROJECT*.  Most composition related functions use *PROJECT* 
	by default.
	   
**(LP &optional name)**

    LP is a convenience version of LOAD-PROJECT with a few additional
    tricks.
	
	- The optional name argument defaults to the last project loaded.
	- LP remembers the last project even after CYCO has been exited and restarted.
	  
**(LOAD-PROJECT-FILE name)**

    Loads a Lisp file relative to the current project directory.  All project
	filenames are converted to lower-case.
	  
**(LPF &optional name)**

    Convenience version of LOAD-PROJECT-FILE
	
	name defaults to the most recently loaded project file.  This is useful 
	while working interactively to quickly reload a file currently under
	development. 
	

## Anatomy of the main project file

Briefly the primary tasks of the main project file are:

- Create a PROJECT object.
- Define an orchestra
- Load section files
- Set section play order
- Generate MIDI files

### Creating the PROJECT Object

The project object is in general responsible for holding everything together
and for establishing default values.

**(MAKE-PROJECT name &key tempo bars beats subbeats unit cuefn shuffle title catalog-number remarks main-file make-current output-directory project-directory)**
<br>

**(PROJECT name &key tempo bars beats subbeats unit cuefn shuffle title catalog-number remarks main-file make-current output-directory project-directory)**

The MAKE-PROJECT function is nearly identical to the PROJECT macro.  The
only difference is that PROJECT binds the new project object to a symbol
named name while MAKE-PROJECT does not. <br> 
The name argument for MAKE-PROJECT should be a quoted symbol, for PROJECT
it should be unquoted. <br>

The CYCO prompt is updated to include the current project's name.



    :tempo             - Float, project tempo in BPM, default 60.
    :bars              - Int, number of bars per phrase, default 4.
    :beats             - Int, number of beats per bar, default 4.
    :subbeats          - Int, number of subbeats per beat, default 4.
    :unit              - Symbol, project time-signature unit, default 'Q
    :cuefn             - Function used for time specifications, default #'BAR
	:shuffle           - Function determines shuffle amount, default #'NO-SHUFFLE
    :title             - String, optional title, defaults to name.
    :catalog-number    - String, optional solely for user's use.
    :remarks           - String, optional solely for user's use.
    :main-file         - String, specify an alternate project main file.
    :make-current      - Boolean, if true bind project object to *PROJECT*, default t.
    :project-directory - String, defaults to *DEFAULT-PROJECT-DIRECTORY*
    :output-directory  - String, defaults to *DEFAULT-PROJECT-OUTPUT-DIRECTORY*

    By default the newly created project object is bound to the global variable 
    *PROJECT*.  Set :make-current to nil to suppress binding to *PROJECT*
    
	
	See time-signature
	See cue and shuffle functions.
	
### Defining an Orchestra

CYCO does not have an 'orchestra' object perse, instead the term implies
the tree of instruments rooted at \*ROOT-INSTRUMENT\*. <br>  
Instruments fall into two general categories:

1. Permanent (non-transient). These are usually established by configuration
   via the plugin feature and are the same across all projects.
   
2. Temporary (transient).  These are defined specifically by each project.

If the orchestra is simple you may define it directly in the main project
file, otherwise consider using a dedicated orchestra file and load it
with (LPF 'orchestra)

**Regardless of where the orchestra is defined if is very important
to call (PRUNE-ORCHESTRA) before creating it.**<br>

An orchestra definition mostly consist of a series of INSTRUMENT and 
MAKE-INSTRUMENT statements.

Use the **(?O)** function to display the orchestra tree.

See plugins <br>
See instruments

### Loading Section Files

Sections represent major divisions of a composition. In all but the
simplest case sections are defined in their own files and loaded with LPF. 

When a SECTION object is created it is designated as the *current-section*
within it's parent project.  Most section-related functions default to the
current-section of \*PROJECT*\.

### Setting Section Order

Prior to creating a MIDI file you must establish the order in which
sections are played with the SECTION-ORDER function.

**(SECTION-ORDER sections &key (project \*project\*))**

    Sets section play order for project.
	
	The sections argument is a list of section names (as symbols) in the
    order they are to be played.  
	
    	(SECTION-ORDER '(intro verse chorus verse ...))
	
	How a section is played may be modified by embedding it in a sub-list
	with any of the following keyword values.
	
	    :x n          - Repeat n times.
		:trans n      - Transpose by n half-steps.  
		                Sections and Parts whose :TRANSPOSABLE property 
						is nil are not effected.
		:invert pivot - Applies key inversion around pivot key.
		                Sections and Parts whose :TRANSPOSABLE property 
						is nil are not effected.
		:retro flag   - If flag is true play the section in reverse.
		                Sections and Parts whose :REVERSABLE property 
						is nil are not effected.
		:shift time   - Shift start by time seconds.   
		                Normally each section begins immediately after the
						previous section ends.  The shift option may be 
						used to overlap two sections or to create a gap 
						between them.  Overlapping two section should be
						used with caution, particularly if they contain
						events on the same MIDI channel.
						
	    (SECTION-ORDER '(intro (verse :x 2) chorus (verse :x 2 :trans 3) ...))
		
### Rendering Projects to a MIDI File


**(PROJECT->MIDI &key (project \*PROJECT\*)(filename nil))**

    Writes project events to MIDI file.
	
	:project  - Sets project to render, defaults to *PROJECT* 
	:filename - Sets filename relative to the project's output directory. 
	            Defaults to the project's name.
				
	For project Foo the resulting file is ~/cyco-projects/foo/MIDI/foo.mid
	
### Example Project File 

    ;; ~/cyco-projects/foo/foo-main.lisp
    
    (version 3)                       ;; Enforce specific CYCO version
    (project foo :tempo 120 :bars 2)  ;; Create project object, bind it to *PROJECT*,
                                      ;; sets default tempo and bar count.
    (prune-orchestra)                 ;; Clear orchestra of old instruments.
    (lpf 'orchestra)                  ;; Load project file defining orchestra
    (lpf 'verse)                      ;; Load section files...
    (lpf 'chorus) 
    (lpf 'bridge)
    (section-order '((verse :x 2) chorus bridge verse)) 
    (project->midi) 
     

### Miscellaneous Comments.

- Additional rendering targets, such as music-xml, may be implemented in the future.
- Individual sections may also be rendered to MIDI files.
  These are often more useful then a full project render.
- The **(?P)** and **(?O)** functions print the current project and 
  orchestra trees respectively.
- **(?)** is a general help function, try **(? \*PROJECT*)** to see 
  more details about the current project structure. 
