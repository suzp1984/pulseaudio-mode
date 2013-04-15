pulseaudio-mode
===============

A pulseaudio management mode for emacs

INSTALL:

add following line into your emacs's init file

1. (add-to-list 'load-path "directory path to pulseaudio-autoload.el")
2. (require 'pulseaudio-autoload)

TODO:
  - add a control panel 
  - add a volume control
  - add a sinks/sources/sink-inputs/source-inputs manager mode.
  - add a special functional mode to record a sink-inputs.
  
Tutorial:

1. pulseaudio modules management
   M-x list-pulseaudio-modules

2. pulseaudio sinks management
   M-x list-pulseaudio-sinks
