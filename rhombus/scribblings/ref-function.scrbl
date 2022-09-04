#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(val dots: @rhombus(..., ~bind))
@(val dots_expr: @rhombus(...))

@title{Functions}

An expression followed by a parenthesized sequence of expressions is
parsed as an implicit use of the @rhombus(#{#%call}) form, which is
normally bound to implement function calls.

@doc(
  expr.macro '$fun_expr #{#%call} ($arg, ..., $maybe_rest)',

  grammar arg:
    $arg_expr
    $keyword: $arg_expr,

  grammar maybe_rest:
    $repetition $$(@litchar{,}) $$(dots_expr)
    $$("ϵ")
){

  A function call. Each @rhombus(arg_expr) alone is a by-position
  argument, and each @rhombus(keyword: arg_expr) combination is a
  by-keyword argument. If the argument sequence ends with @dots_expr,
  the then the elements of the preceding @tech{repetition} are used as
  additional by-position arguments, in order after the
  @rhombus(arg_expr) arguments.

 @see_implicit(@rhombus(#{#%call}), @rhombus(()), "expression", ~is_infix: #true)

@examples(
  List.length([1, 2, 3]),
  List.length #{#%call} ([1, 2, 3])
)

}

@doc(
  defn.macro 'fun $identifier_path($kwopt_binding, ..., $maybe_rest) $maybe_result_annotation:
                $body
                ...',
  defn.macro 'fun
              | $identifier_path($binding, ..., $maybe_rest) $maybe_result_annotation:
                  $body
                  ...
              | ...',

  expr.macro 'fun ($kwopt_binding, ..., $maybe_rest) $maybe_result_annotation:
                $body
                ...',

  expr.macro 'fun
              | ($binding, ..., $maybe_rest) $maybe_result_annotation:
                  $body
                  ...
              | ...',

  grammar identifier_path:
    $identifier
    $identifier_path . $identifier,

  grammar kwopt_binding:
    $binding
    $keyword: $binding
    $binding $$(@tt{=}) $default_expr
    $keyword: $binding $$(@tt{=}) $default_expr,
  
  grammar maybe_result_annotation:
    :: $annotation
    -: $annotation
    $$("ϵ"),

  grammar maybe_rest:
    $binding $$(@litchar{,}) $$(dots)
    $$("ϵ")
){

 Binds @rhombus(identifier_path) as a function, or when
 @rhombus(identifier_path) is not supplied, serves as an expression that
 produces a function value.

 See @secref("namespaces") for information on @rhombus(identifier_path).

@examples(
  fun f(x):
    x+1,
  f(0),
  fun List.number_of_items(l):
    List.length(l),
  List.number_of_items(["a", "b", "c"])
)

@examples(
  ~label: #false,
  val identity: fun (x): x,
  identity(1),
)

@examples(
  ~label: #false,
  fun curried_add(x):
    fun(y):
      x + y,
  curried_add(1)(2)
)

 When @litchar{|} is not used, then arguments can have keywords and/or
 default values. Bindings for earlier arguments are visible in each
 @rhombus(default_expr), but not bindings for later arguments;
 accordingly, matching actions are interleaved with binding effects (such
 as rejecting a non-matching argument) left-to-right, except that the
 result of a @rhombus(default_expr) is subject to the same constraints
 imposed by annotations and patterns for its argument as an explicitly
 supplied argument would be.

@examples(
  fun f(x, y = x+1):
    [x, y],
  f(0),
  f(0, 2),
)

@examples(
  ~label: #false,
  fun transform([x, y],
                ~scale: factor = 1,
                ~dx: dx = 0,
                ~dy: dy = 0):
    [factor*x + dx, factor*y + dy],
  transform([1, 2]),
  transform([1, 2], ~dx: 7),
  transform([1, 2], ~dx: 7, ~scale: 2)
)

 When alternatives are specified with multiple @litchar{|} clauses, the
 alternatives are tried in order when the function is called. The
 alternatives can differ by number of arguments as well as annotations
 and binding patterns.

@examples(
  fun | hello(name):
          "Hello, " +& name
      | hello(first, last):
          hello(first +& " " +& last),
  hello("World"),
  hello("Inigo", "Montoya"),
)

@examples(
  ~label: #false,
  fun | is_passing(n :: Number): n >= 70
      | is_passing(pf :: Boolean): pf,
  is_passing(80) && is_passing(#true)
)

When a @rhombus(rest) is specified as @rhombus(binding $$(@litchar{,}) $$(dots)),
then the function alternative accepts any number of additional
argument, and each variable in @rhombus(binding) is bound to a
repetition for all additional arguments, instead of a single argument.

@examples(
  ~label: #false,
  fun
  | is_sorted([]): #true
  | is_sorted([head]): #true
  | is_sorted([head, next, tail, ...]):
      head <= next && is_sorted([next, tail, ...]),
  is_sorted([1, 2, 3, 3, 5]),
  is_sorted([1, 2, 9, 3, 5])
)

}

@doc(
  defn.macro 'operator ($operator_path $binding) $maybe_result_annotation:
                $body
                ...',
  defn.macro 'operator ($binding $operator_path $binding) $maybe_result_annotation:
                $body
                ...',
  defn.macro 'operator
              | ($operator_path $binding) $maybe_result_annotation:
                  $body
                  ...
              | ($binding $operator_path $binding) $maybe_result_annotation:
                  $body
                  ...',

){

 Binds @rhombus(operator_path) as an operator, either prefix or infix.
 The @rhombus(maybe_result_annotation) parts are the same as in
 @rhombus(fun) definitions.

 When an operator definition includes both a prefix and infix variant
 with @litchar{|}, the variants can be in either order.

 See @secref("namespaces") for information on @rhombus(operator_path).

@examples(
  operator (x ^^^ y):
    x +& y +& x,
  "a" ^^^ "b",
  operator (x List.(^^^) y):
    x ++ y ++ x,
  begin:
    import: .List open
    [1, 2] ^^^ [3]
)
  
}
