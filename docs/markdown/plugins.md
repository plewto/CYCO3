# CYCO Plugins

Plugins are simply bits of Lisp code with a few requirements for
file locations and names.   By default plugins are located in one of two
locations:


1. cyco3/plugins/  The default plugins shipped with CYCO.
2. ~/.config/cyco/plugins/  Personal plugins.

These locations are established in the file cyco3/src/locations.lisp 

A plugin named 'foo' must meet the following conditions.

1. A directory with the plugin's name must exists in either of the
   locations above. 
   
2. The directory and and names of all files it contains must be lowercase
   only. 
   
3.  The main plugin file is named foo-main.lisp


     ~/.config/cyco/foo/
                    |
                    +-- foo-main.lisp
                    |
                    +-- additional lisp files...
                    
                

**(PLUGIN name &optional reload)**

    Loads the named plugin.
	
	name should be an unquoted symbol.
	
	Once a plugin has been loaded it is not reloaded unless the optional
	reload flag is true.
	
    A plugin may load another plugin 
	
**(LOAD-PLUGIN-FILE name)**

    Loads a lisp file relative to the current plugin. 
	
	(PLUGIN foo)
	
	;; loads the file ~/.config/cyco/plugins/foo/foo-main.lisp
		
	(LOAD-PLUGIN-FILE 'alpha)

	;; Loads ~/.config/cyco/plugins/foo/alpha.lisp
	
**(FORGET-PLUGINS)**

    Forgets all currently loaded plugins.  It is important to note that the
	plugins are not actually unloaded from memory; they are still very
	much active after a call to (FORGET-PLUGINS).   All this call does is
	removes the record of them having been loaded so they may be reloaded
	by calling (PLUGIN ...).
	
**(?PLUGINS)**

    Prints a list of all plugin names.  An asterisk to the left of a
	plugin's name indicates it has been loaded.
	
	
