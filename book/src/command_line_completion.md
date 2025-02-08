# Shell completions

Werk supports dynamic shell completion of arguments and tasks.
See below for how to enable them in your shell.



## Registration 

**Bash**

```bash
source <(COMPLETE=bash werk)
```

**Zsh**

```zsh
source <(COMPLETE=zsh werk)
```

**Fish**

```fish
COMPLETE=fish werk | source
```
To enable completions automatically, insert the line into `.config/fish/completions/werk.fish`. [^note]


[^note]: Note that the communication between `werk` and the shell is not stable, so you should not write the output of `COMPLETE=<shell> werk` directly into the completion file (see clap issue [#3166](https://github.com/clap-rs/clap/issues/3166))