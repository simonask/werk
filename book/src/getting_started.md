# Getting Started

`werk` is installed from source, and prebuilt binaries are not provided at this
time. That said, `werk` is a self-contained binary that can easily be
transferred between machines.

## Installation Dependencies

- `rustc` >= 1.83.0

## Installation steps

1. `git clone https://github.com/simonask/werk`
2. `cd werk`
3. `cargo install --path werk-cli`

## Running

If Cargo is configured properly on your system, your `$PATH` should already
contain the path to Cargo-installed binaries.

* Cargo installs binaries in `$CARGO_HOME/bin`
* On UNIX-like systems, the default install location is `$HOME/.cargo/bin`
* On Windows, the default install location is `%USERPROFILE%\.cargo\bin`

Verify that `werk` is installed correctly by running `werk --help`.

## Language support for VS Code

If you are using Visual Studio Code, there is a simple language extension
providing syntax highlighting in the `werk-vscode` subdirectory.

* From the command-line: `code --install-extension <path-to-werk-vscode>`
* From within VS Code: Run "Developer: Install Extension from Location..." and
    point it to the path to the `werk-vscode` directory within this
    repository.

## Other IDEs

If your IDE supports `.sublime-syntax` definition files (such as Sublime Text),
point your IDE to `werk.sublime-syntax` in the repository's root to add syntax
highlighting support.
