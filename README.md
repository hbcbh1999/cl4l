# cl4l
#### esoteric CL essentials

### welcome
Welcome to cl4l. The project supports asdf and may be loaded by calling ```(ql:quickload "cl4l")``` once cl4l.asd is in the asdf search path. ```(cl4l-test:run-suite nil)``` runs all tests.

### memoization
```memoize.lisp``` implements general purpose memoization of arbitrary, potentially parameterized expressions.

### coroutines
```coro.lisp``` implements poor man's coroutines using conditions and restarts.

### indexes
```index.lisp``` implements ordered, optionally composite key/transacted/unique indexes that offer significantly (currently hovering above 10x, but there should be plenty of low hanging fruit left) better performance for set operations.

### tables
```table.lisp``` implements optionally composite key/transacted tables with index-/update and i/o support.

### tests
```test.lisp``` implements a test framework using tags to group and trigger tests dynamically.

### license
MIT

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
