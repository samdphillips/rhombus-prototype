#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "set"){Sets}

When @litchar("{")...@litchar("}") is used with elements that do not
have @rhombus(:), then @litchar("{")...@litchar("}") creates a set. (If
a set-element expression uses @rhombus(:), then it will need to be in
parentheses to avoid being parsed as a key–value pair.) A set can serve
as a map, where the set's elements act as keys and each key's value is
@rhombus(#true). There's a @rhombus(Set) constructor that's analogous to
@rhombus(Map), but @rhombus(Set) accepts just values to include in the
set. The @rhombus(++) operator effectively unions sets.

@(rhombusblock:
    val friends: {"alice", "bob", "carol"}

    if friends["alice"] && friends["carol"]
    | "I know both"
    | "Who are they?"
    // prints "I know both"

    val new_friends: friends ++ {"david"}
    new_friends["david"]  // prints #true
    friends["david"]      // prints #false
  )

Using @rhombus{Set} explicitly before @litchar("{")...@litchar("}")
disables the special treatment of @rhombus(:) to indicate a map, and
each element within @litchar("{")...@litchar("}") is simply an
expression. The @rhombus{Set} constructor can also be used like a
function with @litchar("(")...@litchar(")") instead of
@litchar("{")...@litchar("}").

@rhombus(Set.of) and @rhombus(MutableSet) work as you'd expect. When
@litchar{[}...@litchar{]} with @rhombus(:=) is used to modify a mutable
set, the ``key'' is removed from the set if the assigned value is
@rhombus(#false), otherwise the ``key'' is added to the set.
