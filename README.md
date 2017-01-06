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

aThis extension let you open lync chat window directly from mu4e.
Note that because it's is implemented only by open URI like
`sip:xxx@yyy.com`, so technically the registed application for
`sip` will be opened(which on Windows will usually be Lync).

Way to use:

1. Move cursor to any contacts in `mu4e:view` mode
2. Press `L` to open Lync chat window with the contact


mu4e-goodies-signature-switch
-----------------------------

This extension provides simple signature switch function for mu4e.

Way to use:

1. Put signatures you may want to use to `mu4e-goodies-signatures`
2. Press `Ctrl-c s`(predefined key-binding) to switch between
   signatures when composing emails in mu4e


mu4e-goodies-hacks
------------------

This extension provides some hacks to mu4e to change some
default behaviors of mu4e.

1. Allow a mu4e-view buffer detached from mu4e-header so that it will be
   retained in a seperated window or frame. To use this function, press
   `'` under mu4e-view mode.
2. Always put attachements to the bottom of mail
2. *TODO* Remove duplicated signatures and mail headers which are very common
   in mails sent by Outlook before sent.



mu4e-goodies-actions
--------------------

Some actions maybe useful to you.

1. Show the whole thread of current email. This is bound to `o` in
   headers and message view by default.
2. View the current email's html part by mu4e-html2text-command. This
   is bound to `t` in message view.
3. Quick search all emails sent by current email's sender. This is
   bound to `x` in message view.
4. *TODO* Lync with all contacts in the current email. This is bound
   to `L` in message view.
5. Create org todo/meeting from current message. This is bounded to
   `n/m` in message view

mu4e-goodies-keyword-alert
--------------------------

Inform you when your mail contains some keywords (like attachment)
while the coordinate feature is not found (like the mail doesn't get
an attachment).

By far, the extension support 2 kinds of check:

1. `check-attach`: Whether or not the mail has an attachment
2. `check-cc`: Whether or not the mail has at least one Cc recipients

To use this extension, you may have to customize the variable
`mu4e-goodies-keywords`.





