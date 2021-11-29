# HMX

For the final project we implement an extensible visual text editor
like Emacs.

Milestone: As for 11/29, we have implemented a Lisp parser and
interpreter, and a myriad of Lisp primitives. The editor now supports
moving the cursor and inserting text.

Architecture: the editor is made of a Lisp interpreter and a display
front-end. Program state like buffer content and point position is
stored in the Lisp environment as Lisp values. The display front-end
render the view according to the state, and passing events to the Lisp
world.

The primary challenge is to implement peripheral language features
that are good enough to write Lisp code in, like error report. It is
also challenging to communicate between the front-end and Lisp
interpreter with minimal machinery.

We expect to meet our goal at the deadline—we already have a
functional editor, the only work left is to add more features, like
delete, select, copy & paste, etc.
