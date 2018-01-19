J* (JStar)
==============

A multi-paradigm, functional-first, dynamically typed, general-purpose programming language.  

Influenced by:
- JavaScript
- Lua
- OCaml
- F#

Foreign Function Interface
--------------------------

Simply use functions from any C-Library with `extern`-Function:

```
let printfn = extern('printf');
printf('The answer is: %i', 42);
```

Link the library:

```
jstarc my-code.jss --lib stdio --libpath /usr/local/lib
```

TODOS
-----

Parser
------
- Lexial Analysis (done)
- Symbolic Analysis
- Grammar
- AST

Module System
-------------

- JSTAR_PATH, JSTAR_MODULES
- Node.JS-style

Runtime
-------

- Console - stdout, stderr
- System
- Filesystem
- FFI (C) - we want a simple interoparability with C
- Garbadge Collection / Memory Management
- Processes via Actor-Pattern
- Serialization (JSON, YAML, XML, ...)

Backend
-------

- Evaluate: LLVM, NekoVM, CLR, JVM(?)
- Convert AST to Backend-Code
- Optimizations
- Tail-Call-Optimization

