# Arrays / lists

Werk has first-class support for lists. Elements of lists can be accessed using
the subscript operator `[index]`, where `index` is either a constant integer or
an expression producing the string representation of an integer.

When array indices are negative, the result is the element from the end of the
array. For example, `-1` gets the last element of the list, `-2` gets the
next-to-last element, and so on.

Subscript operators may also appear within string interpolations.

Example:

```werk
let my-list = ["a", "b", "c"]
let a = my-list[0]          # "a"

let my-index = "1"
let b = my-list[my-index]   # "b"

let c = my-list[-1]         # "c"
```

## Array operations

These operations are specific to arrays, but arrays may also appear in other
operations. See [Expressions](./expressions.md).

### `len`

Get the number of elements in a list (as a string). When passed a string, this
always returns 1.

Example:

```werk
let my-list = ["a", "b", "c"]
let len = my-list | len        # "3"
```

### `first`

Get the first element of a list, or the empty string if the list is empty. This
is different from `array[0]` in that it does not raise an error when the list is
empty.

Example:

```werk
let my-list = ["a", "b", "c"]
let first = my-list | first    # "a"

let empty = [] | first         # ""
```

### `last`

Get the last element of a list, or the empty string if the list is empty. This
is different from `array[-1]` in that it does not raise an error when the list
is empty.

Example:

```werk
let my-list = ["a", "b", "c"]
let last = my-list | last      # "c"

let empty = [] | last          # ""
```

### `tail`

Produce a new list with the first element removed, or an empty list if the list
is empty.

Example:

```werk
let my-list = ["a", "b", "c"]
let tail = my-list | tail     # ["b", "c"]
```
