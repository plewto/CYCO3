;;;; CYCO rcmp plugin docs.
;;;;

(in-package :rcmp)

(constant +DOCS+ "
rcmp is a terminal based MIDI file player with remote OSC access, it may be
obtained from https://github.com/plewto/rcmp

You must start rcmp separately from CYCO, at that time you may select a MIDI
device for output.   After starting, the only way to communicate with rcmp
is via OSC messages.  

rcmp constructs a media-list by scanning a directory for MIDI files.  You
then select an entry from the list for playback.  The following functions
are imported into the main CYCO package by the rcmp plugin.

(rcmp:exit)
    Terminate the rcmp program.

(rcmp:help)
    Display this message

(rcmp:scan directory)
    First clears the media-list, then adds all MIDI files in directory to
    the media-list.   The first file is automatically selected for
    playback.   NOTE: It can take rcmp several seconds to refresh after
    scanning a directory.

(rcmp:rescan)
    Rescan the current directory.  The currently selected media is
    re-selected. 

(rcmp:select name)
    Selects an item from the media-list.  name may either by the numeric
    position in the media-list or the items name. 

(rcmp:?media)
    Causes rcmp to display the current media list.   Note the list is
    displayed in the terminal rcmp is executing in and not in CYCO.

(rcmp:play)
    Start playback of the selected media item.

(rcmp:stop)
    Stop playback.  It can take a second or 2 for rcmp to actually halt
    playback.")

