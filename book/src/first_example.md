# My first Werkfile

When running `werk`, it looks for a `Werkfile` in the current directory, and all
its parent directories.

Create a `Werkfile` in a directory with the following contents:

```werk
default target = "hello"

task hello {
    info "Hello, World!"
}
```

Run `werk` in the directory:

```sh
$ werk
[info] Hello, World!
[ ok ] hello
```
