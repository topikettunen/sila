# Syntax

> **Note**: syntax written in here is mainly for designing what the syntax I
> would like to have. This means that it isn't necessarily implemented in code
> as so. Also, this syntax might change at least in the early phases of
> development at will.

## Comments

- `#` for comments.

- `#| |#` for block comments (like Common Lisp).

## Expression

Everything written in Sila should be an expression and have a result. This is
mainly for the reason that I can set e.g. variables with syntax like:

```
a : String := if <cond> { "TRUE" } else { "FALSE" }
```

Where depending on the `<cond>`, `a` would result to either `"TRUE"` or `"FALSE"`.

Or similarly:

```
a : [Int] := for i in 1..10 { i }
```

Where `a` would equal to an array of integers from 1 to below 10.

## Variables and Constants

Variables are constant by default:

```
<ident> : <type> := <value>
```

Assigning a new value to a constant variable, should result to an error.

If you want to mutate the variable, add `var` in front of it:

```
var <ident> : <type> := <value>
<ident> = <new value>
```

## Built-in Types

- `Bool` (`True`, `False`)

- `Int`

- `Float`

- `String`

- `Void`

## Operators

- Postfix `?` (e.g. `zero?`) for boolean checks

- `not`, like `!`

- Unary `+` and `-`, e.g. `+10` and `-10`

- `+`, `-`, `*`, `/`

- `+=`, `-=`, `*=`, `/=`

- `==`

- `<>` (not equal)

- `>=`, `<=`

- `and`, `or`

- `:=` constant/variable init

- `=` assigning new value to an variable

## Blocks

```
if <cond> {
  expr1
  expr2
  ...
}
```

`;` can be used to separate expression in single line:

```
if <cond> { expr1; expr2; }
```

## Functions

```
SampleFunction(Parameter1 : <type>, Parameter2 : <type>) : <type> => {}
```

Block can be omitted:

```
SampleFunction(Parameter1 : <type>, Parameter2 : <type>) : <type> => expr
```


Default parameters can be defined like:

```
SampleFunction(Parameter : <type> := <value>) : <type> => {}
```

## Control Flow

### `if`

```
if <cond> {} else if <cond> {} else {}
```

### `case`

```
case x {
  100 -> "BIG HUNDO"
  _   -> "Not a hundo..."
}
```

### `loop` and `break`

a.k.a. Endless loop

```
loop {}
```

Break out of with `break`:

```
loop {
  expr1
  if x > 10 {
    break
  }
  expr2
}
```

### `for`

```
for x in 1..10 {}
```

```
for x in <array> {}
```

```
for k, v in <map> {}
```

```
for x, y in (x, y) {}
```

### `defer`

Delay the execution of given expression until the current scope exits:

```
defer <expr>
```

```
Func() : Void => {
  expr1
  defer expr2 # expr2 won't be executed here.
  expr3
  ...
} # expr2 executes after leaving this function.
```

## Concurrency

Under construction

## Containers

Under construction

## Composite Types

Under construction
