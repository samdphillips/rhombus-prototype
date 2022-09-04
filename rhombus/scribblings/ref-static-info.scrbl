#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@title{Static Information}

@doc(
  defn.macro '«statinfo.macro '$identifier': 
                $body
                ...»',
){

 Binds @rhombus(identifier) in the static-information space to
 associate the static information produced by the @rhombus(body) block.
 This static information applies to a use of @rhombus(identifier) in the
 expression space. The static information produced by the
 @rhombus(body) block must be in unpacked form (i.e.,
 @rhombus(statinfo_meta.pack) is applied automatically).

 See @secref("annotation-macro") for an example.

}

@doc(
  fun statinfo_meta.wrap(expr_stx:: Syntax,
                         statinfo_stx :: Syntax) :: Syntax
){

 @provided_meta()

 Returns a syntax object for an expression equivalent to
 @rhombus(expr_stx), but with the static information
 @rhombus(statinfo_stx) attached. The @rhombus(statinfo_stx)
 information must be in unpacked form (i.e.,
 @rhombus(statinfo_meta.pack) is applied automatically).

 See @secref("annotation-macro") for an example.

}

@doc(
  fun statinfo_meta.pack(statinfo_stx :: Syntax) :: Syntax
){

 @provided_meta()

 Converts static information described by @rhombus(statinfo_stx) into
 an opaque internal format. The given @rhombus(statinfo_stx) must
 match the form

@(rhombusblock:
   (($$(@rhombus(key_id, ~var)), $$(@rhombus(val, ~var))), ...))

 Keys for static information are compared based on binding, not merely
 the key's symbolic form.

 See @secref("annotation-macro") for an example.

}

@doc(
  fun statinfo_meta.unpack(statinfo_stx :: Syntax) :: Syntax
){

 @provided_meta()

 The inverse of @rhombus(statinfo_meta.pack). This function is
 potentially useful to parse a result from
 @rhombus(staticinfo_meta.lookup) for a key whose value is packed
 static information.

}

@doc(
  fun statinfo_meta.lookup(expr_stx :: Syntax, key :: Syntax)
){

 @provided_meta()

 Finds static information for @rhombus(expr_stx), which might be an
 identifier with static information associated to its binding or an
 expression with static information associated directly. The result is
 a syntax object for the value associated to @rhombus(key) in tat
 static information, or @rhombus(#false) if no information or value
 for @rhombus(key) is available.

 Keys for static information are compared based on binding, not merely
 the key's symbolic form.
 
}

@doc(
  val statinfo_meta.call_result_key,
  val statinfo_meta.ref_result_key,
  val statinfo_meta.map_ref_key,
  val statinfo_meta.map_set_key,
  val statinfo_meta.map_append_key,
  val statinfo_meta.dot_provider_key
){

 @provided_meta()

 Values that can be used to associate static information with an
 expression:

 @itemlist(

  @item{@rhombus(statinfo_meta.call_result_key) --- packed static
        information for the result value if the expression is used as
        a function to call},

  @item{@rhombus(statinfo_meta.ref_result_key) --- packed static information
        for the result value if the expression is used with
        @rhombus([]) to access an element},

  @item{@rhombus(statinfo_meta.map_ref_key) --- an identifier bound to a
        function to call (instead of falling back to a generic dynamic
        dispatch) when the expression is used with @rhombus([]) to
        access an element},
        
  @item{@rhombus(statinfo_meta.map_set_key) --- an identifier bound to a
        function to call (instead of falling back to a generic dynamic
        dispatch) when the expression is used with @rhombus([]) to
        update an element},
        
  @item{@rhombus(statinfo_meta.map_append_key) --- an identifier bound to a
        function to call (instead of falling back to a generic dynamic
        dispatch) when the expression is used with @rhombus(++) to
        append the result of another expression},

  @item{@rhombus(statinfo_meta.dot_provider_key) --- an identifier
        bound to a @rhombus(dot.macro) or
        @rhombus(dot.macro_more_static) to implement the expression's
        behavior as a @tech{dot provider}}

)

 See @secref("annotation-macro") for examples using some of these keys.

}
