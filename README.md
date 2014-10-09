Goodies for mu4e
================

[mu4e](https://github.com/djcb/mu) (mu for emacs)is a
full-feature email client runs inside emacs. Here are some
extensions/hacks of mu4e used by myself.

mu4e-goodies-indicators
-----------------------

This extension add indicators to mode-line to show the status of
the following 3 variables:

1. `mu4e-headers-full-search`
2. `mu4e-headers-show-threads`
3. `mu4e-headers-include-related`

mu4e-goodies-lync
-----------------

This extension let you open lync chat window directly from mu4e.
Note that because it's is implemented only by open URI like
`sip:xxx@yyy.com`, so technically the registed application for
`sip` will be opened(which on Windows will usually be Lync).

Way to use:

1. Move cursor to any contacts in `mu4e:view` mode
2. Press `L` to open Lync chat window with the contact


mu4e-goodies-signature-switch
-----------------------------

This extension provide simple signature switch function for mu4e.

Way to use:

1. Put signatures you may want to use to `mu4e-goodies-signatures`
2. Press `Ctrl-c s`(predefined key-binding) to switch between
   signatures when composing emails in mu4e


















