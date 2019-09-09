# CYCO

CYCO is an expressive music composition language using Lisp.  Scores are
prepared in Lisp and then *rendered* to standard MIDI files.  CYCO is 
project-centric with a rich set of pattern types for algorithmic
compositions and takes a functional approach to event-time specification. 

## History
Around 2008 I developed a set of extensions for [Common Music2](http://commonmusic.sourceforge.net/) 
called CMU (Common Music Utility).  CMU primarily added a project level
framework and an extended notion for time-signatures.  Eventually Common Music
moved to version 3 which invalidated CMU and it became increasingly
difficult to maintain the legacy code.  As I was only using a small subset
of Common Music's features (namely the patterns and the MIDI file
utilities), I decided to sever dependency on Common Music and start from
scratch.  The name CYCO is a mutation of a common pattern type: the Cycle.  


CYCO is written in 100% Lisp and was developed and tested with
[SBCL](http://www.sbcl.org/) 


## Features

- Project centric design.
- Flexible functional time-signature support.
- A rich set of algorithmic pattern generators.
- Symbolic names for common music parameters.
- Chord tables.
- Hierarchical instrument definitions, IE a snare may be a sub-instrument of a
  drum-machine. 
- Plugins
- Under SBCL CYCO's state may be saved as an executable. 
  
## Installation

Other then a functional Lisp the basic CYCO installation has no external
dependencies.  A planned OSC plugin will require an external library but
this is optional.  


1. Place cyco3 distribution folder in a convenient location. <br>
   The default is ~/dev/cyco3 <br>
2. Create the following directories:

       ~/.config/cyco/
	   ~/.config/cyco/plugins/
	   ~/cyco-projects/
	  
       If you elect to use alternate locations modify the file
       cyco3/src/local-config.lisp accordingly. 
   
3. In a terminal navigate to the cyco3 directory and fire up your favorite
   Lisp.
   
4. At the Lisp prompt enter <br><br> 
   **(LOAD "src/cyco")** <br><br>
   
5. After the source files load enter the command <br><br> 
   **(CYCO)** <br><br> 
   A banner should appear and the Lisp prompt changes to CYCO: <br><br>

6. At this point you may run the test-suite by entering: <br><br> 

   **(LOAD "test/cyco-test")** <br><br>
   
   If you run the test, reload CYCO before proceeding. <br><br>
   
7. CYCO is now ready for use.  However you may wish to customize it to
   match the available MIDI equipment.  You do this by creating a plugin
   in the ~/.config/cyco/plugins/ directory as detailed elsewhere.  <br><br>
   
   The tutorials use [General MIDI](https://en.wikipedia.org/wiki/General_MIDI) <br><br>
   If you wish to use General MIDI enter the following command: <br><br>
   
   **(plugin general-midi)** <br><br>
   
8. If you are using SBCL you have the option of saving the current state of
   CYCO to an executable.  <br><br>
   
   **(SAVE-SNAPSHOT executable-name)**  <br><br>
   
   Snapshots are considerably faster then running from source.  After
   starting CYCO from a snapshot you must first enter **(CYCO)** at the initial
   Lisp prompt. 
