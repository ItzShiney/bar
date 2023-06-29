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

```rs
// puts `12.3` number in `a`
12.3 > a

// puts "Hello, world!" string in `a`
"Hello, world!" > a

// copies value from `b` to `a`
b > a
```

If the output variable (`a`) does not exist, it gets created

### References

Everything is always being passed by reference. Meaning `b > a` does not actually copy `b`'s value, it just makes `a` and `b` the same variable
`*` operator is used to dereference, i.e. to actually update the variable's value
All literal-writing operations make a new object each time they are executed, meaning in loops they do not produce the same object

```rs
1 > a
a > b

2 > *a
print(a) // 2
print(b) // 2, because they are essentially the same variable
```
```rs
1 > a
a > b

2 > a // creates another value equal to `2` and makes `a` hold the reference to it
print(a) // 2
print(b) // 1
```

### Copy

`*` operator is also used to copy values:
```rs
list(1 2 3) > a
pop-at(*a 1) // would pop from the original list, if not `*`
print(a) // list(1 2 3)
```

### Function call

* Arguments are separated by spaces
* An argument can be either a variable or a literal
* Nesting is allowed

```rs
sum(a 1) > c
print(sum(prod(2 3) prod(4 5)))
```

### Function definition

```rs
{ function-name(arg)
    // *function body*
}
```

```rs
{ function-name(arg) > out-var
    // *function body*
    // at the end of a function call, local variable `out-var` is returned
}
```

### Global and local variables

In functions, global variables can be accessed by prefixing variable ident with `@`

```rs
{ change-global-a()
    2 > @a
}

1 > a
change-global-a()
print(@a) // prints `2`
```

### Labels & jumps

```rs
// a label is defined like this
:a

// *some code*

// jumps back to `:a` and executes `*some code*` again
go a
```

```rs
// `go` can also jump forward
// skips `*some code*`
go b

// *some code*

:b
```

```rs
// an infinite loop!
:infinite
go infinite
```

### Branching

```rs
// global `if` variable is used to determine whether the jump should happen
eq(a 5) > @if

// "go if"
// jumps to `:a` if `@if` is true
goif a

// "go if not"
// jumps to `:a` if `@if` is false
goifn a
```

```rs
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