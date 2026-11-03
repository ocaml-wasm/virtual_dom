open Core
open Js_of_ocaml

module Phase = struct
  type t =
    | Capture
    | Bubbling
end

module Capture_and_bubbling = struct
  type 'a t =
    { capture : 'a option [@sexp.option]
    ; bubbling : 'a option [@sexp.option]
    }
  [@@deriving sexp_of]
end

module Target = struct
  type t =
    | Window
    | Element
end

module Make_js_of_ocaml (X : sig
    type event = private #Dom_html.event

    val event_kind : event Js.t Dom.Event.typ
    val target : Target.t
  end) =
struct
  module Impl = struct
    module Input = struct
      module Listener = struct
        type t = X.event Js.t -> unit Ui_effect.t [@@deriving sexp_of]

        let combine f g =
          match f, g with
          | None, None -> None
          | Some f, None -> Some f
          | None, Some g -> Some g
          | Some f, Some g -> Some (fun event -> Ui_effect.Many [ f event; g event ])
        ;;
      end

      type t = Listener.t Capture_and_bubbling.t [@@deriving sexp_of]

      let combine
        { Capture_and_bubbling.capture = capture1; bubbling = bubbling1 }
        { Capture_and_bubbling.capture = capture2; bubbling = bubbling2 }
        =
        { Capture_and_bubbling.capture = Listener.combine capture1 capture2
        ; bubbling = Listener.combine bubbling1 bubbling2
        }
      ;;
    end

    module State = struct
      type t = { mutable listeners : (Dom_html.event_listener_id list[@sexp.opaque]) }
      [@@deriving sexp_of]
    end

    let set ~use_capture ~target f =
      let use_capture = if use_capture then Js._true else Js._false in
      let handler =
        Dom.handler (fun ev ->
          Effect.Expert.handle_js_of_ocaml ev (f ev) ~on_exn:(fun exn ->
            Exn.reraise exn "Unhandled exception raised in effect");
          Js._true)
      in
      Dom_html.addEventListener target X.event_kind handler use_capture
    ;;

    let init { Capture_and_bubbling.capture; bubbling } element =
      let target : #Dom_html.eventTarget Js.t =
        match X.target with
        | Target.Window -> (Dom_html.window :> Dom_html.eventTarget Js.t)
        | Element -> (element :> Dom_html.eventTarget Js.t)
      in
      { State.listeners =
          [ Option.map capture ~f:(set ~use_capture:true ~target)
          ; Option.map bubbling ~f:(set ~use_capture:false ~target)
          ]
          |> List.filter_opt
      }
    ;;

    let destroy _input state _element =
      List.iter state.State.listeners ~f:Dom_html.removeEventListener
    ;;

    let update ~old_input ~new_input:f state element =
      (* if the callback function changes, cancel the old one and re-install *)
      destroy old_input state element;
      let new_state = init f element in
      state.State.listeners <- new_state.listeners
    ;;

    let on_mount = `Do_nothing
  end

  include Hooks.Make_js_of_ocaml (Impl)

  let create phase ~f =
    let input =
      match phase with
      | Phase.Capture -> { Capture_and_bubbling.capture = Some f; bubbling = None }
      | Bubbling -> { capture = None; bubbling = Some f }
    in
    create input
  ;;
end

module Make (X : sig
    type event = private #Browser_types.event

    val event_kind : Js.js_string Js.t
    val target : Target.t
  end) =
struct
  module Impl = struct
    module Input = struct
      module Listener = struct
        type t = X.event Js.t -> unit Ui_effect.t [@@deriving sexp_of]

        let combine f g =
          match f, g with
          | None, None -> None
          | Some f, None -> Some f
          | None, Some g -> Some g
          | Some f, Some g -> Some (fun event -> Ui_effect.Many [ f event; g event ])
        ;;
      end

      type t = Listener.t Capture_and_bubbling.t [@@deriving sexp_of]

      let combine
        { Capture_and_bubbling.capture = capture1; bubbling = bubbling1 }
        { Capture_and_bubbling.capture = capture2; bubbling = bubbling2 }
        =
        { Capture_and_bubbling.capture = Listener.combine capture1 capture2
        ; bubbling = Listener.combine bubbling1 bubbling2
        }
      ;;
    end

    module State = struct
      type t = { mutable listeners : (Browser_js_types.event_id list[@sexp.opaque]) }
      [@@deriving sexp_of]
    end

    let set ~use_capture ~target f =
      Browser_extra.add_event_listener_with_cleanup
        ~use_capture
        target
        ~type_:X.event_kind
        ~listener:(fun _self ~event ->
          let event = Js.Unsafe.coerce event in
          Effect.Expert.handle event (f event) ~on_exn:(fun exn ->
            Exn.reraise exn "Unhandled exception raised in effect"))
    ;;

    let init
      { Capture_and_bubbling.capture; bubbling }
      (element : #Browser_types.html_element Js.t)
      =
      let target : #Browser_types.event_target Js.t option =
        match X.target with
        | Target.Window ->
          let%map.Option window = Browser_expert.Global.window () in
          (window :> Browser_types.event_target Js.t)
        | Element -> Some (element :> Browser_types.event_target Js.t)
      in
      { State.listeners =
          (match target with
           | Some target ->
             [ Option.map capture ~f:(set ~use_capture:true ~target)
             ; Option.map bubbling ~f:(set ~use_capture:false ~target)
             ]
             |> List.filter_opt
           | None -> [])
      }
    ;;

    let destroy _input state _element =
      List.iter state.State.listeners ~f:Browser_extra.remove_event_listener
    ;;

    let update ~old_input ~new_input:f state element =
      (* if the callback function changes, cancel the old one and re-install *)
      destroy old_input state element;
      let new_state = init f element in
      state.State.listeners <- new_state.listeners
    ;;

    let on_mount = `Do_nothing
  end

  include Hooks.Make (Impl)

  let create phase ~f =
    let input =
      match phase with
      | Phase.Capture -> { Capture_and_bubbling.capture = Some f; bubbling = None }
      | Bubbling -> { capture = None; bubbling = Some f }
    in
    create input
  ;;
end
