# Example: C program

This example shows a very simple project compiling a C program using `werk`. It
showcases using depfiles generated implicitly by `clang`.

src/foo.h:

```c
int foo();
```

src/foo.c:

```c
#include "foo.h"
int foo() { return 123; }
```

src/main.c:

```c
#include "foo.h"
#include <stdio.h>
int main() {
    printf("foo() returned: %d\n", foo());
    return 0;
}
```

Werkfile:

```werk
config default = "build"

# Path to clang
let cc = which "clang"

# Path to linker
let ld = cc

# Build profile (debug or release)
let profile = "debug"

# Pick cflags based on the build profile
let cflags = profile | match {
    "debug" => ["-O0", "-g"]
    "release" => ["-O3"]
    "%" => ""
}

# Build rule for object files
build "%.o" {
    from "{%}.c"
    depfile "{%}.c.d"

    let include-path = "src"
    let flags = [cflags, "-I<include-path>"]

    # Generate depfile and object file in the same command
    run "{cc} -MM -MT <in> -MF <depfile> -c {flags*} -o <out> <in>"
}

# Build rule for the main executable
build "my-program{EXE_SUFFIX}" {
    # Include all .c files in the build
    from glob "src/**/*.c" | map "{:.c=.o}"

    run "{ld} -o <out> <in*>"
}

task build {
    build "my-program{EXE_SUFFIX}"
    info "Build complete!"
}
```
