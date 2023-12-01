# Common Lisp 2023

## Setup for MacOS & VSCode

Install the standard distribution of (Steel Bank Common Lisp)

```sh
brew install sbcl
```

Install the "Alive" extension for REPL editing in VSCode

```sh
code --install-extension rheller.alive
```

Install [Quicklisp](https://www.quicklisp.org/beta/) for installing [Alive's dependencies](https://github.com/nobody-famous/alive#extension-requirements).

```sh
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
$ sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
```

Then run `sbcl` and install Alive's dependencies.

```cl
(ql:quickload "bordeaux-threads")
(ql:quickload "usocket")
(ql:quickload "cl-json")
(ql:quickload "flexi-streams")
```

### Useful Shortcuts
- <kbd>Alt</kbd> <kbd>Shift</kbd> <kbd>Enter</kbd> Send text to the REPL
- <kbd>Alt</kbd> <kbd>Shift</kbd> <kbd>L</kbd> Send current file to the REPL
- <kbd>Alt</kbd> <kbd>Shift</kbd> <kbd>E</kbd> Inline evaluation

# More Resources
- https://learnxinyminutes.com/docs/common-lisp/
