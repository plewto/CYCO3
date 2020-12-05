# Creating a CYCO Project.

#### PROJECTS ROOT

All projects are located within the common directory stored in
the global variable \*PROJECTS-ROOT\*. By default this location
is ~/cyco-projects/.


#### Project Directory Structure

- All project lisp filenames must be lower-case.

- A projects top-level directory must have the same name as the project, 
  and must also be lower-case.

- At a minimum a projects directory must contain two items:

1. A main-file responsible for creating the project object and
   loading the remainder of the project.  The name for this file
   is **projectname-main.lisp**
   
2. A MIDI directory where all generated MIDI files are stored. 


A bare bones project named 'Foo' has the following structure.

     ~/cyco-projects/
        |
        +-- foo/
             |
             +-- foo-main.lisp
             +-- MIDI/


The following global variables control the location and names
for project files.


- \*PROJECTS-ROOT\*
- \*PROJECT-MAIN-FILENAME-FORMAT\*
- \*DEFAULT-PROJECT-OUTPUT-DIRECTORY\*


As a convenience the **CREATE-PROJECT-FRAMEWORK** function may
be used to build a basic project directory.  

Simply pass it the projects name and the appropriate files are created in the
projects-root directory.


#### Loading a project

Entering  **(LP project-name)** loads the main-file of the named project.
Do not quote the name argument.  Subsequently calling LP without an argument reloads the project.  

The default project-name is persistent between CYCO runs.  Entering (LP)
after starting CYCO loads the most recently used project.  

**NOTE: see BUG 0008, for unknown reasons the persistent
filename is sometimes corrupted.**


#### Loading Project files

Within a project the **LPF** function loads a file relative to the projects
directory.

The filename should be quoted and it is not necessary to include the
filename extension.  

Like LP, LPF when called without an argument reloads the most recently
loaded file.  This is a convenience for reloading a file currently under
development without a minimum of typing.


#### Inspection functions

CYCO defines several functions for inspecting various objects.  All of
these functions begin with a question mark.

The function **?** is the most general.  It may take any argument type and
will always displays at least some information about it.  

The **??** function prints a list of all these inspection functions.

