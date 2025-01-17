# Why not `$toolname`?

Here's a loose collection of reasons that I prefer `werk` to other similar
tools:

- `ninja`: Too low-level, not nice to write by hand, very specialized for C/C++.
- `scons`: Very clunky in my opinion, annoying Python runtime dependency.
- `meson`: Hard to use, integrates poorly with other tools.
- `rake`: Ruby does not work on Windows.
- `cargo xtask`: Solves a different problem, running Rust code at build time.
- `cargo script`: Solves a different problem.
- `cmake`: Very hard to use correctly, extremely hard to debug.
- All the Java tools (`gradle`, `maven`, `bazel`): Too specific to Java
  projects, clunky, and hard to use.
