ElScreen 1.4.6 README file

This is the distribution of ElScreen 1.4.6, released December 2007.
It is an Emacs utility with which you can have multiple screens
(window-configuration) on your GNU Emacs as well as GNU screen on
terminal.

If you use emacs-lisp applications which have many windows (like
Gnus, irchat, Wanderlust, Mew...), ElScreen makes it easy to
switch to a different screen, with its configuration unchanged.
You can also create and kill screen, jump to them, rename the
screen, and so on.

This version is developed on (Carbon) Emacs 22.1.50, with APEL 10.7.


Files
-----
This package should contain the following files:

  elscreen.el     ElScreen version 1.4.6 main file
  README          Introduction to ElScreen 1.4.6 (this file)
  ChangeLog       ElScreen ChangeLog file


Installation
------------
To use ElScreen, APEL is required.  You will find it at

  ftp://ftp.m17n.org/pub/mule/apel/

Then, simply install elscreen.el file in this directory to your
load-path, and put following line into your .emacs:

  (load "elscreen" "ElScreen" t)


Usage
-----
You may use following sequences on ElScreen:

  C-z c
  C-z C-c  Create a new screen and switch to it.
  C-z C    Create a new screen with the window-configuration of the
           current screen.
  C-z k
  C-z C-k  Kill current screen.
  C-z M-k  Kill current screen and buffers.
  C-z K    Kill other screens.
  C-z n
  C-z C-n  Switch to the "next" screen in a cyclic order.
  C-z p
  C-z C-p  Switch to the "previous" screen in a cyclic order.
  C-z a
  C-z C-a  Toggle to the screen selected previously.
  C-z '    Prompt for a screen number to switch to.
  C-z "    Present a list of all screens for selection.
  C-z 0
    :      Jump to the screen number 0-9.
  C-z 9
  C-z C-s  Swap current screen with previous one.
  C-z w
  C-z C-w  Show a list of screen.
  C-z A    Allow the user to enter a name for the current screen.
  C-z m
  C-z C-m  Repeat the last message displayed in the mini-buffer.
  C-z t
  C-z C-t  Show system information.
  C-z b    Switch to the screen in which specified buffer is displayed.
  C-z C-f  Create new screen and open file.
  C-z C-r  Create new screen and open file but don't allow changes.
  C-z d    Create new screen and run dired.
  C-z M-x  Read function name, then call it with new screen.
  C-z i    Show/hide the screen number in the mode line.
  C-z T    Show/hide the tab on the top of each frame.
  C-z v    Display ElScreen version.
  C-z ?    Show key bindings of ElScreen and Add-On softwares.


Setup
-----
You can set the following variables to configure ElScreen.  These
can be set in .emacs file directly or "Options" in your menu bar.

  elscreen-prefix-key

    ElScreen prefix-key.  The default value is `\C-z'.

  elscreen-buffer-to-nickname-alist

    The pairs of buffer-name and corresponding screen nickname or
    function that returns nickname, which are listed by
    'elscreen-display-screen-name-list' only when major-mode cannot
    determine its screen nickname.  The default value is:

        '(("^dired-mode$" .
           (lambda ()
             (format "Dired(%s)" dired-directory)))
          ("^Info-mode$" .
           (lambda ()
             (format "Info(%s)" (file-name-nondirectory Info-current-file))))
          ("^mew-draft-mode$" .
           (lambda ()
             (format "Mew(%s)" (buffer-name (current-buffer)))))
          ("^mew-" . "Mew")
          ("^irchat-" . "IRChat")
          ("^liece-" . "Liece")
          ("^lookup-" . "Lookup"))

  elscreen-mode-to-nickname-alist

    The pairs of major-mode and corresponding screen nickname or
    function that returns nickname, which are listed by
    'elscreen-display-screen-name-list'.  The default value is:

        '(("[Ss]hell" . "shell")
          ("compilation" . "compile")
          ("-telnet" . "telnet")
          ("dict" . "OnlineDict")
          ("*WL:Message*" . "Wanderlust"))

  elscreen-startup-command-line-processing

    If non nil, ElScreen processes command line arguments of Emacsen
    when starting up, and opens files with new screens if needed.  The
    default value is `t'.

  elscreen-display-screen-number

    If non nil, show the number of the current screen in mode
    line.  The default value is `t'.

  elscreen-display-tab

    Specify how the tabs at the top of frame should be displayed.  t
    means to display tabs whose width should be calculated
    automatically.  A value of integer means to display tabs with
    fixed width of this value.   nil means don't display tabs.  The
    default value is `t'.

  elscreen-tab-display-control (only for GNU Emacs 21)

    If non nil, display the tab (labeled with [<->]) to switch to
    next/previous screen or create new screen at the most left side of
    the tab line.  The default value is `t'.

  elscreen-tab-display-kill-screen (only for GNU Emacs 21)

    Location of the icon ([X]) to kill corresponding screen on each
    tab.  Possible values are 'left, 'right and nil (to hide icons).
    The default value is 'left.


Where Can I Get ElScreen?
-------------------------
ElScreen is available from the following anonymous ftp site.

  ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/


Bugs
----
Under multiple-frame environment, screen numbers displayed on mode
line of each frame is changed at the same time.  On GNU Emacs 21, tabs
also has this restriction.


Bug Reports
-----------
ElScreen is maintained by Naoto Morishima.  Please mail bug reports
and any comments to:

       naoto@morishima.net


Acknowledgment
--------------
Many people contributed to ElScreen by reporting problems or suggesting
various improvements.  Here is a list of these people.

  Tohru Sugayama
  Yoshinobu Takenaga
  Masatoshi Takamura
  Jin Kashimura
  Takahiko Sakai
  Norio Suzuki
  Yoshitatsu Takeshita
  Yoichi Nakayama
  sen_ml@eccosys.com
  Dan Debertin
  Yoshinori Koseki
  Hideyuki Shirai
  Masahiro Ishiyama
  Alexy Khrabrov
