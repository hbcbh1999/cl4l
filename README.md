# cl4l
#### esoteric CL essentials

### welcome
Welcome to cl4l. The project supports asdf and may be loaded by calling ```(ql:quickload "cl4l")``` once cl4l.asd is in the asdf search path. ```(cl4l-test:run-suite nil)``` runs all tests.

### memoization
```memoize.lisp``` implements general purpose memoization of arbitrary, potentially parameterized expressions.

### iterators
```iter.lisp``` implements an extensible iterator protocol on top of coroutines using conditions and restarts. They are currently around 10x slower than consing and reversing lists; while supporting infinite lists without freaking out; and side stepping call site confusion about who owns what.

### fifo queues
```fifo.lisp``` implements minimal fifo queues on top of lists.

### tables
```table.lisp``` implements hashed, optionally composite key/transacted tables with set operations, event hooks, index-/update and i/o support. Tables are roughly 20% faster than indexes, despite added book keeping for update support.

### indexes
```index.lisp``` implements ordered, optionally composite key/transacted/unique indexes with event hooks and i/o support that offer significantly (currently hovering above 10x) better performance for set operations than built-in set functionality.

### semaphores
```semaphore.lisp``` implements minimal semaphores on top of bordeaux-threads.

### channels
```chan.lisp``` implements optionally buffered channels on top of fifo queues and semaphores.

### tests
```test.lisp``` implements a test framework using tags to group and trigger tests dynamically.

### license
MIT

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
