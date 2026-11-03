open Base
open Js_of_ocaml
include Ui_effect

(* All visibility handlers see all events, so a simple list is enough. *)
let visibility_handlers : (unit -> unit) list ref = ref []

module type Visibility_handler = sig
  val handle : unit -> unit
end

module Define_visibility (VH : Visibility_handler) = struct
  let () = visibility_handlers := VH.handle :: !visibility_handlers
end

module Open_url_target = struct
  type t =
    | This_tab
    | New_tab_or_window
    | Iframe_parent_or_this_tab
    | Iframe_root_parent_or_this_tab

  let to_target = function
    (* https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a#target *)
    | This_tab -> "_self"
    | New_tab_or_window -> "_blank"
    | Iframe_parent_or_this_tab -> "_parent"
    | Iframe_root_parent_or_this_tab -> "_top"
  ;;
end

type _ t +=
  | Viewport_changed
  | Stop_propagation
  | Stop_immediate_propagation
  | Prevent_default
      [@deprecated
        "[since 2026-02] use [event##preventDefault] instead, which is available on \
         [Dom_html.event Js.t]"]
  | Open :
      { url : string
      ; target : Open_url_target.t
      }
      -> unit t

let sequence_as_sibling left ~unless_stopped =
  let rec contains_stop = function
    | Many es -> List.exists es ~f:contains_stop
    | Stop_immediate_propagation -> true
    | _ -> false
  in
  if contains_stop left then left else Ui_effect.Many [ left; unless_stopped () ]
;;

let open_url ?(in_ = Open_url_target.This_tab) url = Open { url; target = in_ }

(* We need to keep track of the current dom event here so that movement between
   [Vdom.Effect.Expert.handle] and [Ui_concrete.Effect.Expert.handle] keeps the original
   dom event around. *)
let current_dom_event = ref None

let () =
  Hashtbl.add_exn
    Expert.handlers
    ~key:Stdlib.Obj.Extension_constructor.(id (of_val Viewport_changed))
    ~data:(fun _ -> List.iter !visibility_handlers ~f:(fun f -> f ()))
;;

let () =
  Hashtbl.add_exn
    Expert.handlers
    ~key:Stdlib.Obj.Extension_constructor.(id (of_val Stop_propagation))
    ~data:(fun _ ->
      Option.iter !current_dom_event ~f:(fun (event : #Browser_types.event Js.t) ->
        event##stopPropagation))
;;

let () =
  Hashtbl.add_exn
    Expert.handlers
    ~key:
      Stdlib.Obj.Extension_constructor.(
        id (of_val (Prevent_default [@alert "-deprecated"])))
    ~data:(fun _ ->
      Option.iter !current_dom_event ~f:(fun (event : #Browser_types.event Js.t) ->
        event##preventDefault))
;;

let () =
  Hashtbl.add_exn
    Expert.handlers
    ~key:(Stdlib.Obj.Extension_constructor.id [%extension_constructor Open])
    ~data:(fun hidden ->
      let window = Browser_expert.Global.window_exn () in
      let url, target, callback =
        match hidden with
        | T { value = Open { url; target }; callback; on_exn = _ } ->
          url, target, fun () -> callback ()
        | _ -> failwith "Unrecognized variant"
      in
      let (_ : Browser_types.window Js.t Js.Opt.t) =
        window##open_
          ~url:(Js.string url |> Js.Optdef.return)
          ~target:(Js.string (Open_url_target.to_target target) |> Js.Optdef.return)
          ~features:Js.Optdef.empty
      in
      callback ())
;;

module Expert = struct
  let handle_non_dom_event_exn t =
    Expert.handle t ~on_exn:(fun exn ->
      Exn.reraise exn "Unhandled exception raised in effect")
  ;;

  let handle ~on_exn ?on_further_exns dom_event event =
    let old = !current_dom_event in
    current_dom_event := Some (dom_event :> Browser_types.event Js.t);
    Expert.handle ~on_exn ?on_further_exns event;
    current_dom_event := old
  ;;

  let handle_js_of_ocaml ~on_exn ?on_further_exns dom_event event =
    let dom_event = Browser_extra.Coercion.of_jsoo_event dom_event in
    handle ~on_exn ?on_further_exns dom_event event
  ;;
end
