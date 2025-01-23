# Built-in variables

## Variables

In build recipes:

- `in`, or `{^}` in strings: Input files (the result of the `from` statement)
- `out`, or `{@}` in strings: Output files (the path of the actual file being
  built by the recipe).
- `depfile`: If the recipe has a `depfile` statement, this is the evaluated
  [path](../paths.md) to the depfile.
- `%` or `{%}` in strings: The stem of the matched pattern, if a pattern is in
  scope and that pattern has a `%` in scope. When defining patterns in a scope
  where another pattern is already present, the interpolated `{%}` may be used
  to unambiguously refer to the stem of the "outer" pattern.
- `{+}` or `<+>`: Same as `in`, except that duplicate entries are preserved in
  the order they were declared. This is primarily useful for use in linking
  commands where it is meaningful to repeat library file names in a particular
  order.
- `{@D}`: The directory part of the file name of the target. Same as
  `{out:dir}`.
- `{@F}`: The file-within-directory part of the file name of the target. Same as
  `{out:filename}`.

## Global constants

These variables are valid in all scopes.

- `ROOT`: The abstract or filesystem path to the project root, which is always
  `/`. When interpolated as `<ROOT>`, this becomes the filesystem path to the
  directory containing the Werkfile.
- `OS`: Lowercase name of the host operating system.
  - Windows: `windows`
  - macOS: `macos`
  - Linux: `linux`
  - FreeBSD: `freebsd`
  - DragonFly: `dragonfly`
  - OpenBSD: `openbsd`
  - NetBSD: `netbsd`
  - WebAssembly (WASIp2): `wasm-wasi`
- `OS_FAMILY`: Classification of the host operating system.
  - Windows: `windows`
  - Linux, macOS, and BSDs: `unix`
  - WebAssembly (WASIp2): `wasm`
- `ARCH`: Name of the host architecture.
  - x86_64 / x64: `x86_64`
  - x86: `x86`
  - ARM (64-bit): `aarch64`
  - ARM (32-bit): `arm`
  - WebAssembly: `wasm`
- `ARCH_FAMILY`: Classification of the host architecture.
  - x86, x86_64: `x86`
  - ARM: `arm`
  - WebAssembly: `wasm`
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
- `COLOR`: When color output is enabled for `werk`, this is set to `"1"`. This
  may be used to conditionally pass command-line arguments to compilers that
  don't respect the conventional `CLICOLOR` environment variables.
