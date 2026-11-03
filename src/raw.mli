open Base
open Js_of_ocaml

module Attrs : sig
  type t = private Js.Unsafe.any

  val create : unit -> t
  val has_property : t -> Js.js_string Js.t -> bool
  val has_attribute : t -> Js.js_string Js.t -> bool
  val set_property : t -> Js.js_string Js.t -> Js.Unsafe.any -> unit
  val set_attribute : t -> Js.js_string Js.t -> Js.Unsafe.any -> unit
end

module Node : sig
  type t

  val node : Js.js_string Js.t -> Attrs.t -> t Js.js_array Js.t -> string option -> t
  val text : string -> t
  val svg : Js.js_string Js.t -> Attrs.t -> t Js.js_array Js.t -> string option -> t
  val to_dom : t -> Browser_types.html_element Js.t
  val to_dom_js_of_ocaml : t -> Dom_html.element Js.t
end

module Patch : sig
  type t

  val create : previous:Node.t -> current:Node.t -> t
  val apply : Browser_types.html_element Js.t -> t -> Browser_types.html_element Js.t
  val apply_js_of_ocaml : Dom_html.element Js.t -> t -> Dom_html.element Js.t
  val is_empty : t -> bool
end

module Widget : sig
  type t = Node.t

  val create
    :  ?vdom_for_testing:Node.t Lazy.t
    -> ?destroy:('s -> 'browser Js.t -> unit)
    -> ?update:('s -> 'browser Js.t -> 's * 'browser Js.t)
    -> id:
         ('s * #Dom_html.element Js.t * (#Browser_types.html_element as 'browser) Js.t)
           Type_equal.Id.t
    -> init:(unit -> 's * 'browser Js.t)
    -> unit
    -> t

  val create_js_of_ocaml
    :  ?vdom_for_testing:Node.t Lazy.t
    -> ?destroy:('s -> (#Dom_html.element as 'a) Js.t -> unit)
    -> ?update:('s -> 'a Js.t -> 's * 'a Js.t)
    -> id:('s * 'a Js.t * #Browser_types.html_element Js.t) Type_equal.Id.t
    -> init:(unit -> 's * 'a Js.t)
    -> unit
    -> t
end
