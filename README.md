# Dotfiles for Emacs


## A note about keybinding
On some terminals [certain key sequences are not recieved by Emacs](http://stackoverflow.com/questions/4623630/emacs-control-shift-up-doesnt-work).
For instance, it may be that Ctrl-Shift-c (`C-S-c`) is seen as just
Ctrl-c (`C-c`) by Emacs.  You can check what sequences are send by
typing key sequence `Ctrl-v <more>` in the terminal.  Example:
```sh
$ [Ctrl-v Ctrl-Shift-c]
```
If you see
```sh
$ ^C
```
then you know that only Ctrl-c (`^C`) is received.


## License
The files I create are released under LGPL (>= 2.1).  For all other
files, please see their individual licenses; they are mostly
distributed under GPL.
