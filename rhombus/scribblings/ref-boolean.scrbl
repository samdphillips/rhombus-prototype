#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Booleans}

@doc(
  annotation.macro 'Boolean'
){

  Matches @rhombus(#true) or @rhombus(#false)

}

@doc(
  expr.macro '$expr || $expr'
){

 Produces the value of the first @rhombus(expr) if it is
 non-@rhombus(#false), otherwise produces the value(s) of the second
 @rhombus(expr).

 The second @rhombus(expr) is evaluated in tail position with respect to
 the @rhombus(||) form.

}

@doc(
  bind.macro '$left_bind || $right_bind'
){

 Matches if either @rhombus(left_bind) or @rhombus(right_bind) matches.
 No identifiers are bound after a successful match, however.

@examples(
  fun check_shape(v):
    match v
    | [x] || [x, y, z]: #true
    | ~else: #false,
  check_shape([1]),
  check_shape([1, 2, 3]),
  check_shape([1, 2])
)

}

@doc(
  expr.macro '$expr && $expr'
){

 Produces @rhombus(#false) if the the value of the first @rhombus(expr)
 is @rhombus(#false), otherwise produces the value(s) of the second
 @rhombus(expr).

 The second @rhombus(expr) is evaluated in tail position with respect to
 the @rhombus(&&) form.

}

@doc(
  bind.macro '$left_bind && $right_bind'
){

 Matches when both @rhombus(left_bind) and @rhombus(right_bind) match.
 All identifiers from bindings are available after the match, and
 static information from @rhombus(left_bind) is propagated to
 @rhombus(right_bind) (but not the other way around).

@examples(
  class Posn(x, y),
  fun three_xs(v):
    match v
    | [a, b, c] && [Posn(x, _), ...]:
        [x, ...]
    | ~else: #false,
  three_xs([Posn(1, 2), Posn(3, 4), Posn(5, 6)]),
  three_xs([Posn(1, 2), Posn(3, 4)]),
  three_xs([Posn(1, 2), Posn(3, 4), "no"]),
)

}

@doc(
  operator (! v):: Boolean
){

 Returns @rhombus(#true) if @rhombus(v) is @rhombus(#false),
 @rhombus(#false) otherwise.


@examples(
  !#false,
  !#true,
  !"false"
)

}
