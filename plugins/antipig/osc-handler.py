#! /usr/bin/python3
#
# OSC handler for antipig plugin.
#
# osc-handler.py is an intermediate communication step between CYCO 
# and Pigiron.  
#
# CYCO calls osc-handler.py via the Lisp run-program function.
# The handler in turn sends an OSC command to Pigiron.  sys.argv  must at
# least contain a command name to be run.  Some commands take mandatory or
# optional arguments. 
#
# Commands:
#    ping                             - Transmits diagnostic 'ping'
#    load midi-file-name              - Loads specified MIDI file
#    stop                             - Stops playback
#    play                             - Starts playback
#    play-main                        - Loads and plays main project MIDI file
#    play-section [optional-name]     - Loads and plays current section's MIDI file
#    out-channels ...                 - Selects MIDI output channels
#    off                              - Disables all MIDI output channels
#    mon [state]                      - Enables/Disables monitor
#

import sys, time
import os.path as path
from pythonosc.udp_client import SimpleUDPClient
from pythonosc import dispatcher

ip = "127.0.0.1"
port = 8020

temp_dir = "/tmp/antipig"
sleep_delay = 1.0

def sleep():
    time.sleep(sleep_delay)

def get_current_project():
    fname = path.expanduser(path.join(temp_dir, "current-project"))
    with open(fname, "r") as fobj:
        name = fobj.readline()[:-1]
        return name

def get_midi_directory():
    fname = path.expanduser(path.join(temp_dir, "midi-directory"))
    with open(fname, "r") as fobj:
        name = fobj.readline()[:-1]
        return name

def get_current_section():
    fname = path.expanduser(path.join(temp_dir, "current-section-midifile"))
    with open(fname, "r") as fobj:
        name = fobj.readline()[:-1]
        return name    

def echo(message, value):
    frmt = "OSC Transmitted %s port %s message '%s' %s"
    values = (ip, port, message, value)
    print(frmt % values) 

def transmit(message, value):
    echo(message, value)
    client = SimpleUDPClient(ip, port)
    client.send_message(message, value)

def ping():
    transmit("/pig/ping", "")

def pig_load(name):
    transmit("/pig/op", ["player", "load", name])

def stop():
    transmit("/pig/op", ["player", "stop"])

def play():
    transmit("/pig/op", ["player", "play"])

def pigout(channels):
    acc = ["distributor"]
    for c in channels:
        acc.append(c)
    transmit("/pig/select-channels", acc)

def pigoff():
    transmit("/pig/deselect-all-channels", ["distributor"])

def play_main():
    main_filename = get_current_project() + "-main.mid"
    directory = get_midi_directory()
    midi_filename = path.join(directory, main_filename)
    pig_load(midi_filename)
    sleep()
    play()

def play_section(sname=None):
    if not sname:
        sname = get_current_section()
    root, ext = path.splitext(sname)
    if not ext:
        sname = root + ".mid"
    directory = get_midi_directory()
    midi_filename = path.join(directory, sname)
    pig_load(midi_filename)
    sleep()
    play()


def monitor(flag):
    if flag == "on":
        transmit("/pig/op", ["mon", "enable", "true"])
    else:
        transmit("/pig/op", ["mon", "enable", "false"])
    
# argv   <ignore>, command, [args...]
#
if __name__ == '__main__':
    argv = sys.argv
    if len(argv) < 2:
        print("ERROR: OSC-HANDLER.PY did not receive mandatory command")
    else:
        cmd = argv[1]
        if cmd == "ping":
            ping()
        elif cmd == "load":
            pig_load(argv[2])
        elif cmd == "stop":
            stop()
        elif cmd == "play":
            play()
        elif cmd == "out-channels":
            pigout(argv[2:])
        elif cmd == "off":
            pigoff()
        elif cmd == "play-main":
            play_main()
        elif cmd == "play-section":
            sname = ""
            if len(argv) > 2:
                sname = argv[2]
            play_section(sname)
        elif cmd == "mon":
            monitor(argv[2])
        else:
            print(f"OSC-HANDLER.PY  invalid command: {cmd}")
