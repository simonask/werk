# Why not just?

[just](https://just.systems/) is a command runner that is very popular. It fits
the niche where what you need is a collection of "housekeeping" tasks for your
project.

It's very easy to use, and has a syntax inspired by Make, but it isn't able to
actually build things (tracking dependencies between artifacts), only run
commands.

It also comes with a shell requirement, making it hard to write portable
Justfiles.
