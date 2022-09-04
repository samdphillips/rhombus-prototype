#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title(~style: symbol(toc)){Rhombus Meta and Macros Reference}

@docmodule(rhombus/meta)

@docmodule(~lang, ~no_declare, rhombus/and_meta)

Simple pattern-based expression macros can be written using
@rhombus(def) without imorting any additional libraries besides
@rhombusmodname(rhombus), but implementing others forms of macros requires
using the @rhombusmodname(rhombus/meta) module (usually with no prefix).

The @rhombusmodname(rhombus/meta) module provides bindings like
@rhombus(defn.macro), @rhombus(expr.macro), @rhombus(expr.rule), and
it also re-exports all of @rhombusmodname(rhombus) as @rhombus(meta, ~impmod)
for use in compile-time expressions.

The @rhombusmodname(rhombus/and_meta) module provides all of the
bindings from both @rhombusmodname(rhombus) and
@rhombusmodname(rhombus/meta). It's intended for use as a language---an
alternative to starting with @rhombusmodname(rhombus) and importing
@rhombusmodname(rhombus/meta).

@local_table_of_contents()

@include_section("ref-meta.scrbl")
@include_section("ref-defn-macro.scrbl")
@include_section("ref-expr-macro.scrbl")
@include_section("ref-bind-macro.scrbl")
@include_section("ref-annotation-macro.scrbl")
@include_section("ref-dot-provider.scrbl")
@include_section("ref-static-info.scrbl")
@include_section("ref-stxobj-meta.scrbl")
