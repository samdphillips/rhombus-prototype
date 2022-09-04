#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "operator"){Operators}

The @rhombus(operator) form defines a prefix or infix operator for
expressions, similar to a function definition:

@(rhombusblock:
    operator (x <> y):
      Posn(x, y)

    1 <> 2  // prints Posn(1, 2)

    operator (<<>> x):
      Posn(x, x)

    <<>> 3  // prints Posn(3, 3)
  )

An ``operator'' name does not have to be a shrubbery operator. It can be
an identifier:

@(rhombusblock:
    operator (x mod y):
      x - floor(x / y) * y

    10 mod 3  // prints 1
  )

The @rhombus(operator) form must be followed by parentheses and then a
block. Inside the parentheses, there must be exactly two or three terms,
and the next-to-last term must be an operator or identifier to define.
The arguments can be described by binding patterns, but in that case,
they may need parentheses around the pattern to ensure that they form a
single term in next to the operator being defined:

@(rhombusblock:
    operator ((x :: Integer) <> (y :: Integer)):
      Posn(x, y)

    // 1 <> "apple"  // would be a run-time error
  )

An operator can be defined for both infix and prefix behavior in much
the same way that functions can be defined to accept one or two
arguments:

@(rhombusblock:
    operator
    | ((x :: Integer) <> (y :: Integer)):
        Posn(x, y)
    | (<> (x ::Integer)):
        Posn(x, x)

    1 <> 2  // prints Posn(1, 2)
    <> 3    // prints Posn(3, 3)
  )

Operator precedence is declared in relationship to other operators when
the operator is defined. With no precedence defined, @rhombus(<>) cannot
appear near an arithmetic operator like @rhombus(*):

@(rhombusblock:
    // 1 <> 2 * 3  // would be a syntax error
    1 <> (2 * 3)   // prints Posn(1, 6)
  )

The initially defined operators mostly have the usual precedence:
@rhombus(*) and @rhombus(/) are stronger than @rhombus(+) and
@rhombus(-), while @rhombus(+) and @rhombus(-) have the same predence
and are left-associative. The @rhombus(*) and @rhombus(/) operator have
the same precedence as long as @rhombus(*) appears only to the left of
@rhombus(/),

A precedence declaration in @rhombus(operator) takes the form of keyword
blocks at the start of the operator’s body. The possible keyword options
for prefix operators are @rhombus(~weaker_than),
@rhombus(~stronger_than), @rhombus(~same_as), or
@rhombus(~same_as_on_left). For infix operators, those options apply, as
well as @rhombus(~same_as_on_right) and @rhombus(~associativity).
Operators listed with keywords like @rhombus(~weaker_than) can be
grouped on lines however is convenient.

@(rhombusblock:
    operator (x <> y):
      ~weaker_than: * / 
                    + -
      ~associativity: ~right
      Posn(x, y)

    1 <> 2 * 3  // prints Posn(1, 6)
    1 <> 2 <> 3 // prints Posn(1, Posn(2, 3))
  )

Use the keyword @rhombus(~other) in @rhombus(~weaker_than),
@rhombus(~stronger_than), or @rhombus(~same_as) to declare a precedence
relationship for operators not otherwise mentioned.

An operator can be exported the same as identifiers:

@(rhombusblock:
    export:
      <>
  )

On the import side, to refer to an operator that has a prefix, put the
operator after @rhombus(.) in parentheses:

@(rhombusblock:
    import:
      "posn.rhm"

    1 posn.(<>) 2
  )

If the point of an operator is terseness, an import prefix may defeat
the point. Using a library that supplies operators may be one reason to
import with a leading @rhombus(=) to avoid a prefix on the imports. To
selectively make an operator accessible without it import’s prefix, use
the @rhombus(expose) import modifier:

@(rhombusblock:
    import:
      "posn.rhm":
        expose: <>

    1 <> 2
  )
