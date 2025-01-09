# Built-in variables and constants

These variables are valid in all scopes.

## Variables

In build recipes:

- `in`, or `^` in string interpolations: Input files (the result of the `from` statement)
- `out`, or `@` in string interpolations: Output files (the path of the actual
  file being built by the recipe).
- `depfile`: If the recipe has a `depfile` statement, this is the evaluated path
  to the depfile.

## Global constants

- `OS`: Lowercase name of the host operating system.
  - Windows: `windows`
  - macOS: `macos`
  - Linux: `linux`
- `OS_KIND`: Classification of the host operating system.
  - Windows: `windows`
  - Linux and macOS: `unix`
- `ARCH`: Name of the host architecture.
  - x86_64 / x64: `x86_64`
  - x86: `x86`
  - ARM (64-bit): `aarch64`
  - ARM (32-bit): `arm`
- `ARCH_KIND`: Classification of the host architecture.
  - x86, x86_64: `x86`
  - ARM: `arm`
- `EXE_SUFFIX`:
  - Windows: `.exe`
  - Other: empty
- `DYLIB_PREFIX`:
  - Windows: empty
  - Linux and macOS: `lib`
- `DYLIB_SUFFIX`:
  - Windows: `.dll`
  - Linux: `.so`
  - macOS: `.dylib`
- `STATICLIB_PREFIX`:
  - Windows: empty
  - Linux and macOS: `lib`
- `STATICLIB_SUFFIX`:
  - Windows: `.lib`
  - Linux and macOS: `.a`
- `EMPTY`: Always the empty string
