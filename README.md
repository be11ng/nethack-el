# `nethack-el`

What happens when you combine the most advanced, self-documenting, customizable,
extensible real-time display editor, Emacs, with nethack, the most elaborate
role-playing environment ever invented?

You get the most advanced, elaborate, self-documenting, customizable,
extensible, role-playing environment in the world!

All of the Elisp is GPLv2, and the patches are under the modified 3-clause BSD.

## Features

* Customizable keys
* Customizable colors
* Macros
* Event hooks
* All the beauty that comes with Emacs
* Play on remote `nethack-el` servers

## Background

This is a fork of `nethack-el` where I attempt to update it to 3.6.6.

I found this hunk of Elisp code a while ago, but at the time,
it looked like it dead and hadn't been updated in forever.
Furthermore, it was only updated to NetHack 3.4.3,
and not yet up to the 3.6.4 that I was playing (now 3.6.6).

Old Web: <http://savannah.nongnu.org/projects/nethack-el>

## Build and run

*The following section is nearly copied verbatim from the old INSTALL file.*

I'm working on a better install process, similar to that of `pdf-tools`,
but for now, you'll need to patch, download and make your own NetHack.
These instructions are known to work on \*NIX systems (sorry Windows users).

* Download either `nethack-343.tgz` or `nethack-366.tgz` from
  <https://nethack.org>.

  * 331, 340, 341, 342, and SLASH'EM 007e0 and 007e3 are supported to varying
  extents.

* Untar the package and apply the respective patch

  For example, if you were installing 3.6.6, you would do something like:

  ```
  $ tar xzf nethack-366-src.tgz
  $ cp enh-366.patch NetHack-NetHack-3.6.6_Released
  $ cd NetHack-NetHack-3.6.6_Released
  $ patch -p 1 < enh-366.patch
  ```

* Follow the instructions in `sys/*/Install.*` or `NewInstall.*`

  * This may be as simple as running `sys/unix/setup.sh` to copy the Makefiles.

  * For the 3.6.6 patch, included is a hints file under
    `sys/unix/hints/linux-lisp` based under the default Linux one, which builds
    a NetHack patch into `~/.local/bin`.
    You may want to change this before running something along the lines of:

    ```
    $ cd sys/unix
    $ vi hints/linux-lisp
    $ sh setup.sh hints/linux-lisp
    ```

* Compile (from the toplevel)

  * Following the instructions as outlined so far, that would be:

    ```
    $ cd ../..
    $ make all
    $ make install
    ```

* Install the Elisp sources

  * Edit the Makefile in the `nethack-el` directory to set the location of your
    Emacs.

  * Byte compile (not strictly necessary).

    ```
    $ cd .. # Or wherever nethack-el is located
    $ make all
    ```

  * Place the `*.elc` or `*.el` files in your load-path.

  * Add the following lines somewhere inside your `init.el`:

    ```elisp
    (autoload 'nethack "nethack" "Play Nethack." t)
    (setq nethack-program "/PATH/TO/PATCHED/nethack")
    ```

  * Play with `M-x nethack RET`.

## Play NetHack over a network

Because `nethack-el` communicates with nethack over a text stream, that
stream can be either a subprocess or a TCP stream. Follow these steps
to play `nethack-el` over the internet:

* On the server

  Edit the config values `USER_FILE`, `CHROOT_DIR`, `SHED_UID`, and `SHED_GID`
  in `nhlaunch.c`. Compile `nhlaunch.c` (instructions are at the top of the
  file).

  `USER_FILE` is the file that will store the users and passwords

  `CHROOT_DIR` is the root directory that contains a fakeroot installation of
  nethack and any libraries required.

  `SHED_UID` is the UID that `nhlaunch` will change to when loaded.

  `SHED_GID` is the GID that `nhlaunch` with change to when loaded.

* On the client (in Emacs)

  `M-x nethack-connect-to-server RET`

  Type the server and the port. Enjoy! Now everyone can play on the same server
  using their own Emacs.