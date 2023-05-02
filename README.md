# Bar

A fun programming language without any practical value

## Syntax

### Idents

Valid characters for idents are:
* `a` to `z`
* `-`
* `0` to `9` (not in initial position)

Examples:
* `valid-ident`
* `--another-valid-ident12-34`
* `999_not_a_valid_ident`

### Writing/copying

```rust
// puts `12.3` number in `a`
12.3 > a

// puts "Hello, world!" string in `a`
"Hello, world!" > a

// copies value from `b` to `a`
b > a
```

If the output variable (`a`) does not exist, it gets created

### Function call

* Arguments are separated by spaces
* An argument can be either a variable or a literal
* No nesting is allowed

```rust
sum(a 1) > c
```

### Function definition

```rust
{ function-name(arg)
    // *function body*
}
```

```rust
{ function-name(arg) > out-var
    // *function body*
    // at the end of a function call, local variable `out-var` is returned
}
```

### Global and local variables

In functions, global variables can be accessed by prefixing variable ident with `@`

```rust
{ change-global-a()
    2 > @a
}

1 > a
change-global-a()
print(@a) // prints `2`
```

### Labels & jumps

```rust
// a label is defined like this
:a

// *some code*

// jumps back to `:a` and executes `*some code*` again
go a
```

```rust
// `go` can also jump forward
// skips `*some code*`
go b

// *some code*

:b
```

```rust
// an infinite loop!
:infinite
go infinite
```

### Branching

```rust
// global `if` variable is used to determine whether the jump should happen
eq(a 5) > @if

// "go if"
// jumps to `:a` if `@if` is true
goif a

// "go if not"
// jumps to `:a` if `@if` is false
goifn a
```

```rust
{ print-1-to-10()
    1 > i
    :loop
        le(i 10) > @if
        goifn loop-end

        print(i)

        sum(i 1) > i
        go loop
    :loop-end
}
```