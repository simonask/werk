# TODO

- [ ] Take command-line `--define`s into account in outdatedness.
- [ ] Take `werk.toml` modification time into account in outdatedness.
- [ ] Consider using pattern syntax for glob patterns instead of standard glob syntax.
- [ ] Forward doc comments from TOML to `--list`.
- [ ] Don't use `RUST_LOG` to enable logging, as it interferes with child
  processes. Use `WERK_LOG` instead.