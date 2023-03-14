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
# ping                           : Transmits diagnostic ping.
# midi-on                        : Enables MIDI input.
# midi-off                       : Disables MIDI input.
# input-channel <channel>        : Selects MIDI input channel.
# clear-output-channels          : Disables all MIDI output channels.
# add-output-channels <channel>  : Enables a MIDI output channel. 
# display-output-channels        : Display list of enabled output channels.
# load <filename>                : Instructs player to load a MIDI file.
# stop                           : Stops playback.
# play                           : Starts playback (A MIDI file must be loaded first)
# play-section                   : Starts playback for the current CYCO section.
# play-main                      : Starts playback of the current CYCO project's main MIDI file.
# mon <state>                    : Disables/Enables MIDI monitor.  state is either 'false' or 'true'.

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

def midi_off():
    transmit("/pig/enable", ["filter", "false"])

def midi_on():
    transmit("/pig/enable", ["filter", "true"])

def input_channel(c):
    midi_on()
    sleep()
    transmit("/pig/select-channels", ["filter", c])
    
def pig_load(name):
    transmit("/pig/op", ["player", "load", name])

def stop():
    transmit("/pig/op", ["player", "stop"])

def play():
    transmit("/pig/op", ["player", "play"])

def clear_output_channels():
    transmit("/pig/deselect-all-channels", ["distributor"])

def output_channel(channel):
    print(f"PYTHON DEBUG  channels is {channels}")
    no_out()
    transmit("/pig/select-channel", ["distributor", channel])

def add_output_channel(channel):
    transmit("/pig/select-channel", ["distributor", channel])

def display_output_channels():
    transmit("/pig/q-selected-channels", ["distributor"])
    
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
        elif cmd == "midi-on":
            midi_on()
        elif cmd == "midi-off":
            midi_off()
        elif cmd == "input-channel":
            input_channel(argv[2])
        elif cmd == "clear-output-channels":
            clear_output_channels()
        elif cmd == "add-output-channel":
            add_output_channel(argv[2])
        elif cmd == "display-output-channels":
            display_output_channels()
        elif cmd == "load":
            pig_load(argv[2])
        elif cmd == "stop":
            stop()
        elif cmd == "play":
            play()
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
