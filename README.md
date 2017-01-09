# cl4l
#### esoteric CL essentials

### intro
cl4l is a coherent set of Common Lisp utilities designed for maximum leverage.

### setup
The project supports asdf and may be loaded by calling ```(ql:quickload "cl4l")``` once cl4l.asd is in the asdf search path. Calling ```(cl4l-test:run-tests nil)``` runs all tests.

### memoization
```memoize.lisp``` implements support for general purpose memoization of optionally parameterized computations.

### ordered sets
```slist.lisp``` implements ordered, optionally unique sets based on lists.

### indexes
```index.lisp``` implements ordered, composite key indexes with transaction support.

### test
```test.lisp``` implements a testing framework based on the idea of using tags to group and trigger tests dynamically.

### license
MIT

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
