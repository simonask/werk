# `.werk-cache`

This is a special file created by `werk` in the [output
directory](./workspace.md#output-directory).

It is a TOML document containing metadata used during [outdatedness
checks](./outdatedness.md), including [glob](./language/operations.md#glob)
results, used environment variables ([`env`](./language/operations.md#env)),
used program paths ([`which`](./language/operations.md#which)), the recipe
itself, manual command-line overrides (`-Dkey=value`), and any global variables
used while evaluating the recipe.

In short, `.werk-cache` is what enables `werk` do perform very detailed
outdatedness checks.

All values stored in `.werk-cache` are hashed to avoid leaking secrets from the
environment, but the hash is not cryptographically secure. It can't be: since
the hash must be stable between runs, using a random seed would defeat the
purpose.

`.werk-cache` can be safely deleted by the user, but doing so may cause the next
build to rebuild more than necessary.
