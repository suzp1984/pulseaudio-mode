pulseaudio-mode
===============

A pulseaudio management mode for emacs in Linux environment

INSTALL:

install dependency module:

```elisp
    M-x package-install fsm
```

add following line into your emacs's init file

```elisp
    (add-to-list 'load-path "directory path to pulseaudio-autoload.el")
    (require 'pulseaudio-autoload)
```

TODO:
  - add a control panel 
  - add a volume control
  - add a sinks/sources/sink-inputs/source-inputs manager mode.
  - add a special functional mode to record a sink-inputs.
  
Tutorial:

1. pulseaudio modules management

```sh
   M-x list-pulseaudio-modules
```
   
2. pulseaudio sinks management

```sh
   M-x list-pulseaudio-sinks
```

