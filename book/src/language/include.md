# Include

As a project grows, the size of a `Werkfile` may become unwieldy, and it may be
desirable to split recipes and variables into separate files.

Werk supports the `include` statement to evaluate a separate file and include
its variables and recipes in the main `Werkfile`.

Included files are evaluated as-if they were a part of the file that includes
them. For the purposes of expression evaluation, all included files share the
same global scope.

However, [`default` statements](../build_config.md#configure-how-werk-runs) may
only appear in the "main" Werkfile, as they impact how Werk runs.

`include` statements take the form of `include "path/in/workspace.werk"`. The
path may also be an expression, so Werkfiles can selectively include other
sources based on the value of expressions.

## Example

Werkfile:

```werk
include "config.werk"
include "recipes.werk"
```

config.werk:

```werk
config profile = "debug"
```

recipes.werk:

```werk
let cflags = profile | match {
    "debug" => ["-O0", "-g"]
    "release" => ["-O3"]
    "%" => []
}

build "%.o" {
    # ...
}
```

## Advanced example

This example includes a different set of configuration variables based on the
current host platform.

Werkfile:

```werk
config profile = "debug"

include "config_{OS_FAMILY}.werk"
include "recipes.werk"
```

config_windows.werk:

```werk
let cc = which "cl"
let debug_cflags = []
let release_cflags = ["/O"]
```

config_unix.werk:

```werk
let cc = which "clang"
let debug_cflags = ["-O0", "-g"]
let release_cflags = ["-O3"]
```

recipes.werk:

```werk
let cflags = profile | match {
    "debug" => debug_cflags
    "release" => release_cflags
}

build "%.o" {
    from "%.c"
    run "{cc} {cflags*} -o <out> <in>"
}
```
