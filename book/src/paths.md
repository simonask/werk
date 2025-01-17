# Paths

File and directory paths in Werk are not normal paths as understood by the
operating system. This is because one of the primary goals of Werk is to work on
all platforms, and especially equal treatment of poor, maligned Windows.

Paths in Werk are always relative to the workspace root or the output directory.
Files outside of the workspace cannot be treated as inputs to or outputs of
build recipes. Werk is designed to only write to the output directory.

However, invoking external commands often requires passing native OS paths.
Using the [special string interpolation
syntax](./language/strings.md#string-interpolation) `"<var>"`, the abstract path
stored in `var` will be converted to a native absolute path within the
workspace.

Native path resolution may resolve to either an input file in the workspace or a
generated file in the output directory. This check is based on existence: If the
file is found in the workspace, it resolves to the file inside the workspace.
Otherwise, it is assumed that a build recipe will generate the file in the
output directory, and it resolves to an absolute path inside the output
directory, mirroring the directory structure of the workspace.

In general, build recipes should take care to not clobber the workspace and only
generate files with paths that coincide with paths in the workspace.

Logically, the workspace is an "overlay" of the output directory - it always
takes precedence when a file exists, and the output directory is a "fallback".

Consider this directory structure:

```text
c:\
  workspace\
    main.c
    foo.c
    output\
      main.o
```

Here, `c:\workspace\main.c` has previously been built and placed at
`c:\workspace\output\main.o`. However, `foo.c` has not yet been built.

Path resolution will then work like this:

- `/main.c` will resolve to `c:\workspace\main.c` because it exists in the
  workspace.
- `/foo.c` will resolve to `c:\workspace\main.c` because it exists in the
  workspace.
- `/main.o` will resolve to `c:\workspace\output\main.o` because it does not
  exist in the workspace.
- `/foo.o` will resolve to `c:\workspace\output\foo.o` because it does not exist
  in the workspace.
- `/other.c` will resolve to `c:\workspace\output\other.c` because it does not
  exist in the workspace.

## Virtual path rules

- The path separator is forward slash.
- The root path `/` refers to the workspace root, never the native filesystem
  root.
- Path components must be valid UTF-8. Incomplete surrogate pairs on Windows or
  arbitrary byte sequences on Linux/macOS are not supported, and will cause an
  error.
- Path components must only contain "printable" Unicode characters, no control
  characters or newlines.
- Path components must be valid on all platforms. In particular this means that
  the more restrictive rules that apply to paths on Windows also apply to path
  components in abstract paths, even when `werk` is running on other operating
  systems. See [Windows rules](#windows-rules).
- Path components cannot start or end with whitespace.
- Path components cannot end with a period `.` - the filename extension cannot
  be empty.
- Complete paths never end in a path separator.

## Illegal characters

The following characters are illegal in abstract paths paths, and it is a
superset of disallowed paths on Unix-like systems and Windows:

- Shell operators: `<` and `>` and `|`
- Quotation marks: `"` and `'`
- Slashes: `/` and `\`
- Special punctuation characters: `:` and `?` and `*`

## Windows rules

Some file names are reserved on Windows, and may not occur as file names - even
when they also have a file extension, and even in lowercase, or mixed case!

To complete the madness: For the special filenames ending in numbers, the digits
`1`, `2`, and `3` are considered equal to their superscript equivalents. For
example, `COM¹` is reserved in addition to `COM1`.

- `CON`
- `PRN`
- `AUX`
- `COM0`-`COM0`
- `LPT0`-`LPT9`

Werk considers these filenames invalid on _all_ platforms, even when running on
a non-Windows platform. This is to ensure the portability of Werkfiles.
