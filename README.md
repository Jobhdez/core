# Core


`core` is a small compiler that lowers core imperative programming features to runnable x86. It supports: variables, assignment, if statements, while loops.

*note* dev is in progress ...

## Getting Started
**Dependencies**: 
- SBCL: 

MacOS:`brew install sbcl`

Ubuntu `sudo apt-get install sbcl`

Arch Linux `sudo pacman -S sbcl`
- [Quicklisp](https://www.quicklisp.org/beta/)

**Install**:
`git clone git@github.com:Jobhdez/core.git`

Note: clone this project in `quicklisp/local-projects` so you can load the project with `(ql:quickload :core)`.

**Use**:
```
(ql:quickload :core)

(in-package :core
```
**Tests**:
```
(ql:quickload :core/tests)

(asdf:test-system :core)
```
```
## Acknowledgements
This compiler is loosely based on the Python compiler skeleton (written in Python) in the textbook [Essentials of Compilation](https://github.com/IUCompilerCourse/Essentials-of-Compilation). None of the Python code was ported into common lisp. I essentially solved the exercises and wrote my code in a different language.

thanks
