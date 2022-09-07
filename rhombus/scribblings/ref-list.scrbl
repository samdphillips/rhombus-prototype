#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(val dots: @rhombus(..., ~bind))
@(val dots_expr: @rhombus(...))

@title{Lists}

@deftech{Lists} can be constructed using the syntax
@rhombus([$$(@rhombus(expr, ~var)), ...]), which creates list containing the values of the
@rhombus(expr, ~var)s as elements. More precisely, a use of square
brackets without a preceding expression implicitly uses the
@rhombus(#{#%brackets}) form, which (despite its name) is normally bound to
construct a list.

A list works with map-referencing square brackets to access a list
element by position (in time proportional to the position) via
@rhombus(#{#%ref}). A list also works with the @rhombus(++) operator
to append lists.

@dispatch_table(
  "list",
  @rhombus(List),
  [lst.length(), List.length(lst)],
  [lst.first, List.first(lst)],
  [lst.rest, List.rest(lst)]
)

@doc(
  fun List(v :: Any, ...) :: List,
  fun List(v :: Any, ..., repetition, dots) :: List,
  expr.macro '#{#%brackets} [$v_expr]',
  expr.macro '#{#%brackets} [$v_expr, $repetition, dots]',

  grammar dots:
    $$(dots_expr)
){

 Constructs a list of the given @rhombus(v)s values or results of
 the @rhombus(v_expr)s expressions.
 
 When @dots_expr appears at the end, the preceding position is a
 @tech{repetition} position, and all elements of the repetition are
 included in order at the end of the list.

 @see_implicit(@rhombus(#{#%brackets}), @rhombus([]), "expression")

@examples(
  val lst: List(1, 2, 3),
  lst,
  lst[0],
  lst ++ [4, 5],
  #{#%brackets} [1, 2, 3]
)

}

@doc(
  bind.macro 'List($binding, ...)',
  bind.macro 'List($binding, ..., $dots)',
  bind.macro '#{#%brackets} [$binding, ...]',
  bind.macro '#{#%brackets} [$binding, ..., $dots]',
  grammar dots:
    $$(dots)
){

 Matches a list with as many elements as @rhombus(binding)s, or if
 @rhombus(dots) is included, at least as many elements as
 @rhombus(binding)s before the last one, and then them last one is
 matched against the rest of the list.

 @see_implicit(@rhombus(#{#%brackets}, ~bind), @rhombus([]), "binding")

@examples(
  val List(1, x, y): [1, 2, 3],
  y,
  val [1, also_x, also_y]: [1, 2, 3],
  also_y,
  val List(1, x, ...): [1, 2, 3],
  [x, ...]
)

}

@doc(
  annotation.macro 'List',
  annotation.macro 'List.of($annotation)',
){

 Matches any list in the form without @rhombus(of). The @rhombus(of)
 variant matches a list whose elements satisfy @rhombus(annotation).

}

@doc(
  annotation.macro 'NonemptyList',
  annotation.macro 'NonemptyList.of($annotation)',
){

 Like @rhombus(List, ~ann) as an annotation, but matches only non-empty
 lists.

@examples(
  [1] :: NonemptyList,
  ~error [] :: NonemptyList
)

}

@doc(
  reducer.macro 'List'
){

 A @tech{reducer} used with @rhombus(for), accumulates each result of a
 @rhombus(for) body into a result list.

}

@doc(
  fun List.cons(elem :: Any, lst :: List) :: List,
){

 Creates a list like @rhombus(lst), but with @rhombus(elem) added to
 the front.

@examples(
  List.cons(1, [2, 3])
)

}

@doc(
  bind.macro 'List.cons($elem_binding, $list_binding)'
){

 Matches a non-empty list where @rhombus(elem_binding) matches the
 first element of the list and @rhombus(list_binding) matches the
 rest of the list.

@examples(
  val List.cons(x, y): [1, 2, 3],
  x,
  y
)

}

@doc(
  fun List.first(lst :: NonemptyList),
){

 Returns the first element of @rhombus(lst).

@examples(
  List.first(["a", "b", "c"])
)

}

@doc(
  fun List.rest(lst :: NonemptyList) :: List,
){

 Returns a list like @rhombus(lst), but without its first element.

@examples(
  List.rest(["a", "b", "c"])
)

}

@doc(
  fun List.length(lst :: List) :: Integer,
){

 Returns the number of items in @rhombus(lst).

@examples(
  List.length([1, 4, 8]),
  List.length([]),
  [1, 4, 8].length
  )
}
