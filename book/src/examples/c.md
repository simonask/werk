# Example: C program

This example shows a very simple project compiling a C program using `werk`. It
showcases using depfiles generated implicitly by `clang`.

src/foo.h:


```c
{{#include c/src/foo.h}}
```

src/foo.c:


```c
{{#include c/src/foo.c}}
```

src/main.c:

```c
{{#include c/src/main.c}}
```

Werkfile:

```werk
{{#include c/Werkfile}}
```
