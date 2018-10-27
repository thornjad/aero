# thornjad emacs_
   [![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

This is a custom fork of [Spacemacs](https://github.com/syl20bnr/spacemacs). 
Focus is on Tcl, web, Node.js and Lisp development on MacOS and Linux.


## Documentation

Comprehensive documentation is available for each layer by pressing
<kbd>SPC h SPC</kbd>.

Also check Spacemacs's 
[general documentation][https://github.com/syl20bnr/DOCUMENTATION.org], 
[quick start guide][https://github.com/syl20bnr/QUICK_START.org] and 
[FAQ][https://github.com/syl20bnr/FAQ.org].


## Install

### Prerequisites

Spacemacs is an extension of a popular text editor called Emacs. Thus you need
to first install base Emacs and then download the Spacemacs extension files with
Git.

#### Emacs

Spacemacs requires Emacs 25.1 or above. The development version of Emacs is not
*officially* supported, but it should nevertheless be expected to work.

Some modes require third-party tools that you'll have to install via your
favorite package manager.

##### Linux 

Install the `emacs` package with your linux distributions package manager.

Do not install the `xemacs` package. It's not supported by Spacemacs. XEmacs is
an old fork of Emacs. The X in its name is unrelated to X11.

##### macOS

```
brew cask install emacs
brew tap caskroom/fonts
brew cask install font-source-code-pro
```

### Install config

```
git clone https://gitlab.com/thornjad/emacs ~/.emacs.d
```

## License

The license is GPLv3 for all parts specific to Spacemacs, this includes:
- the initialization and core files
- all the layer files
- the documentation

For the packages shipped in this repository, you can refer to the files header.

[Spacemacs logo][] by [Nasser Alshammari][] released under a
[Creative Commons Attribution-ShareAlike 4.0 International License.](http://creativecommons.org/licenses/by-sa/4.0/)


**Support upstream Spacemacs by using the badge in your project**

If you used spacemacs in a project, and you want to show that fact, you can use
the spacemacs badge: [![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

- For Markdown:

   ```
   [![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)
   ```

- For HTML:

   ```
   <a href="http://spacemacs.org"><img src="https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg" /></a>
   ```

- For Org-mode:

   ```
   [[http://spacemacs.org][file:https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg]]
   ```

