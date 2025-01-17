# Werk Build System And Command Runner

`werk` is a simplistic and opinionated command runner, similar to `just`, and
also a simplistic build system, similar to `make`.

You Betta Werk! 💅

> [!CAUTION]
> Werk is early alpha software. Use at your own risk. It may eat your
> files, so run `git commit` before trying it out.

## Why?

GNU make is extremely useful, but very hard to use correctly, especially if you
have modern expectations of your build system, and you just want a convenient
way to execute build scripts, create asset packs, or run housekeeping tasks in
your project.

`just` is also extremely useful, and easy to use, but cannot build files. It can
only run commands, delegating to `make`, `cargo`, or other build systems to
actually produce output. Furthermore, it can be difficult to write
cross-platform `Justfile`s, usually relying on a platform-specific shell
availability.

For mode details, consult the [the book](https://simonask.github.io/werk).

## Installation instructions

`werk` can currently only be installed from source, which requires that you have
Rust and Cargo installed.

* Clone this repository.
* Run `cargo install --path werk-cli`.
* Ensure that your `$PATH` contains the path to Cargo binaries. This is usually
  the case if you have a working installation of Rust and Cargo.
  * Cargo installs binaries in `$CARGO_HOME/bin`.
  * On UNIX-like systems, the default install location is `$HOME/.cargo/bin`.
  * On Windows, the default install location is `%USERPROFILE%\.cargo\bin`.

### Language Support for VS Code

* Clone this repository.
* Install the extension from `werk-vscode`:
  * From the command-line: `code --install-extension <path-to-werk-vscode>`
  * From within VS Code: Run "Developer: Install Extension from Location..." and
       point it to the path to the `werk-vscode` directory within this
       repository.

## Features and limitations

See [Features and limitations](https://simonask.github.io/werk/features.html).

## Project non-goals

* `werk` will probably never be fastest.
    1. User friendliness is always higher priority.
    2. Reporting "no changes" quickly is specifically not a goal. Use Ninja if
    this is important to you. Typically, `werk` is invoked when the user has
    actually made changes.
    3. That said, `werk` does try to be reasonably fast, and is implemented in
    Rust using best practices.
* `werk` will probably never support all use cases.
    1. It is designed to support the use cases that are important to me, the
       author.
    2. The needs of build systems are vast and varied. Use the one that fits
       your purposes, or file a feature request if you believe that `werk` would
       be greater if it could reasonably support it.
* `werk` will never be a scripting language. It is strictly declarative with
  minimal support for logic and expressions, but doesn't have (and won't have)
  loop constructs.

## Examples

See [Examples](./examples).

## Roadmap

* [ ] IDE integration to run individual tasks.
* [ ] WASM host support.
