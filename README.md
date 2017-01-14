# cl4l
#### esoteric CL essentials

### setup
The project supports asdf and may be loaded by calling ```(ql:quickload "cl4l")``` once cl4l.asd is in the asdf search path. Call ```(cl4l-test:run-tests nil)``` to run tests.

### memoization
```memoize.lisp``` implements general purpose memoization of arbitrary, potentially parameterized expressions.

### coroutines
```coro.lisp``` implements poor man's coroutines using conditions and restarts.

### indexes
```index.lisp``` implements ordered, optionally composite key/transacted/unique indexes.

### tables
```table.lisp``` implements optionally composite key/transacted tables with update support.

### tests
```test.lisp``` implements a test framework based on the idea of using tags to group and trigger tests dynamically.

### license
MIT

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
