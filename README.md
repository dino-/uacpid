# uacpid


## Synopsis

Userspace Advanced Configuration and Power Interface event daemon
(Haskell)


## Description

uacpid is a daemon designed to be run in userspace that will monitor
the local system's acpid socket for hardware events. These events can
then be acted upon by handlers with access to the user's environment.

An example of why you need this: Suppose you want to have a hardware
event change the active X displays, like a laptop external monitor
function button. Some tools to achieve this display change require
the logged in user's DISPLAY and other settings that may be difficult
or impossible to gain access to from acpid scripts.

Another example is media control function buttons, like play/pause
or next/previous track for a music player.

uacpid is running as you and is getting the hardware events reported
by the system's acpid. uacpid can then act on the events on your
behalf, with your environment.

uacpid will respond to SIGHUP by reloading the event handlers and
reestablishing its connection to the acpid socket. Changes to the
config file will require that uacpid be restarted, it will not
reload the config on SIGHUP.

Handler syntax closely follows that of acpid. Users familiar with
acpid should have no problem here. An example is included.

Note that uacpid requires acpid. It must be installed and running.

(Please see the uacpid man page for more detailed info) 


## Getting source

- Download the cabalized source package [from Hackage](http://hackage.haskell.org/package/uacpid)
- epub-tools is available for Arch Linux [from the AUR](https://aur.archlinux.org/packages/uacpid/)
- Get the source with darcs: `$ git clone https://github.com/dino-/uacpid.git`
- If you're just looking, [browse the source](https://github.com/dino-/uacpid)

Getting started developing:

This daemon is designed to make files for itself in ~/.uacpid/ the
first time it's run. To assist with development, there is a script
which will build a development installation and execution environment
in /tmp/uacpid-dev/ for you.

To initialize for development:

    $ bin/uacpid-dev-setup


Once that's completed, it can be run like this:

    $ bin/uacpid-dev

And once you have it, building the usual way:

    $ cabal configure
    $ cabal build
    $ cabal install


## Installing

Build and install with cabal-install:
  `$ cabal update ; cabal install uacpid`


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
