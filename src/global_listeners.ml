open Core
open Js_of_ocaml
module Phase = Event_listener.Phase
module Capture_and_bubbling = Event_listener.Capture_and_bubbling
module Target = Event_listener.Target

module Mousedown = Event_listener.Make (struct
    type event = Browser_types.mouse_event

    let event_kind = Browser_extra.Event_type.mousedown
    let target = Target.Window
  end)

module Mouseup = Event_listener.Make (struct
    type event = Browser_types.mouse_event

    let event_kind = Browser_extra.Event_type.mouseup
    let target = Target.Window
  end)

module Mousemove = Event_listener.Make (struct
    type event = Browser_types.mouse_event

    let event_kind = Browser_extra.Event_type.mousemove
    let target = Target.Window
  end)

module Click = Event_listener.Make (struct
    type event = Browser_types.event

    let event_kind = Browser_extra.Event_type.click
    let target = Target.Window
  end)

module Blur = Event_listener.Make (struct
    type event = Browser_types.focus_event

    let event_kind = Browser_extra.Event_type.blur
    let target = Target.Window
  end)

module Focusin = Event_listener.Make (struct
    type event = Browser_types.focus_event

    let event_kind = Browser_extra.Event_type.focusin
    let target = Target.Window
  end)

module Focusout = Event_listener.Make (struct
    type event = Browser_types.focus_event

    let event_kind = Browser_extra.Event_type.focusout
    let target = Target.Window
  end)

module Contextmenu = Event_listener.Make (struct
    type event = Browser_types.pointer_event

    let event_kind = Browser_extra.Event_type.contextmenu
    let target = Target.Window
  end)

module Keydown = Event_listener.Make (struct
    type event = Browser_types.keyboard_event

    let event_kind = Browser_extra.Event_type.keydown
    let target = Target.Window
  end)

module Keyup = Event_listener.Make (struct
    type event = Browser_types.keyboard_event

    let event_kind = Browser_extra.Event_type.keyup
    let target = Target.Window
  end)

module Visibilitychange = Event_listener.Make (struct
    type event = Browser_types.event

    let event_kind = Browser_extra.Event_type.visibilitychange
    let target = Target.Window
  end)

module Beforeunload = Event_listener.Make (struct
    type event = Browser_types.before_unload_event

    let event_kind = Browser_extra.Event_type.beforeunload
    let target = Target.Window
  end)

let mousedown ~phase ~f =
  Mousedown.create phase ~f |> Attr.create_hook "global-mousedown-listener"
;;

let mouseup ~phase ~f =
  Mouseup.create phase ~f |> Attr.create_hook "global-mouseup-listener"
;;

let mousemove ~phase ~f =
  Mousemove.create phase ~f |> Attr.create_hook "global-mousemove-listener"
;;

let click ~phase ~f = Click.create phase ~f |> Attr.create_hook "global-click-listener"
let blur ~phase ~f = Blur.create phase ~f |> Attr.create_hook "global-blur-listener"

let focusin ~phase ~f =
  Focusin.create phase ~f |> Attr.create_hook "global-focusin-listener"
;;

let focusout ~phase ~f =
  Focusout.create phase ~f |> Attr.create_hook "global-focusout-listener"
;;

let contextmenu ~phase ~f =
  Contextmenu.create phase ~f |> Attr.create_hook "global-contextmenu-listener"
;;

let keydown ~phase ~f =
  Keydown.create phase ~f |> Attr.create_hook "global-keydown-listener"
;;

let keyup ~phase ~f = Keyup.create phase ~f |> Attr.create_hook "global-keyup-listener"

class type event_with_string_return_value = object
  (* Events with [returnValue] are impossible to properly type, so we make one that is
     specialized for string, and cast our before_unload type to it. *)
  method returnValue : Js.js_string Js.t Js.writeonly_prop
end

let beforeunload ~phase ~f =
  let f event =
    match%bind.Effect f event with
    | `Show_warning ->
      let event : event_with_string_return_value Js.t = Obj.magic event in
      event##.returnValue := Js.string "this string can be anything";
      Effect.Ignore
    | `Do_nothing -> Effect.Ignore
    | `Custom_best_effort effect -> effect
  in
  Beforeunload.create phase ~f |> Attr.create_hook "global-beforeunload-listener"
;;

let visibilitychange ~phase ~f =
  Visibilitychange.create phase ~f |> Attr.create_hook "global-visibilitychange-listener"
;;

(* Older Js_of_ocaml versions of the above methods *)

module Mousedown_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.mouseEvent

    let event_kind = Dom_html.Event.mousedown
    let target = Target.Window
  end)

module Mouseup_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.mouseEvent

    let event_kind = Dom_html.Event.mouseup
    let target = Target.Window
  end)

module Mousemove_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.mouseEvent

    let event_kind = Dom_html.Event.mousemove
    let target = Target.Window
  end)

module Click_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.mouseEvent

    let event_kind = Dom_html.Event.click
    let target = Target.Window
  end)

module Blur_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.focusEvent

    let event_kind = Dom_html.Event.blur
    let target = Target.Window
  end)

module Focusin_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.focusEvent

    let event_kind = Dom_html.Event.make "focusin"
    let target = Target.Window
  end)

module Focusout_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.focusEvent

    let event_kind = Dom_html.Event.make "focusout"
    let target = Target.Window
  end)

module Contextmenu_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.mouseEvent

    let event_kind = Dom_html.Event.make "contextmenu"
    let target = Target.Window
  end)

module Keydown_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.keyboardEvent

    let event_kind = Dom_html.Event.keydown
    let target = Target.Window
  end)

module Keyup_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.keyboardEvent

    let event_kind = Dom_html.Event.keyup
    let target = Target.Window
  end)

module Visibilitychange_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.event

    let event_kind = Dom_html.Event.make "visibilitychange"
    let target = Target.Window
  end)

module Beforeunload_js_of_ocaml = Event_listener.Make_js_of_ocaml (struct
    type event = Dom_html.event

    let event_kind = Dom_html.Event.make "beforeunload"
    let target = Target.Window
  end)

let mousedown_js_of_ocaml ~phase ~f =
  Mousedown_js_of_ocaml.create phase ~f |> Attr.create_hook "global-mousedown-listener"
;;

let mouseup_js_of_ocaml ~phase ~f =
  Mouseup_js_of_ocaml.create phase ~f |> Attr.create_hook "global-mouseup-listener"
;;

let mousemove_js_of_ocaml ~phase ~f =
  Mousemove_js_of_ocaml.create phase ~f |> Attr.create_hook "global-mousemove-listener"
;;

let click_js_of_ocaml ~phase ~f =
  Click_js_of_ocaml.create phase ~f |> Attr.create_hook "global-click-listener"
;;

let blur_js_of_ocaml ~phase ~f =
  Blur_js_of_ocaml.create phase ~f |> Attr.create_hook "global-blur-listener"
;;

let focusin_js_of_ocaml ~phase ~f =
  Focusin_js_of_ocaml.create phase ~f |> Attr.create_hook "global-focusin-listener"
;;

let focusout_js_of_ocaml ~phase ~f =
  Focusout_js_of_ocaml.create phase ~f |> Attr.create_hook "global-focusout-listener"
;;

let contextmenu_js_of_ocaml ~phase ~f =
  Contextmenu_js_of_ocaml.create phase ~f
  |> Attr.create_hook "global-contextmenu-listener"
;;

let keydown_js_of_ocaml ~phase ~f =
  Keydown_js_of_ocaml.create phase ~f |> Attr.create_hook "global-keydown-listener"
;;

let keyup_js_of_ocaml ~phase ~f =
  Keyup_js_of_ocaml.create phase ~f |> Attr.create_hook "global-keyup-listener"
;;

let beforeunload_js_of_ocaml ~phase ~f =
  let f event =
    match%bind.Effect f event with
    | `Show_warning ->
      let event : event_with_string_return_value Js.t = Obj.magic event in
      event##.returnValue := Js.string "this string can be anything";
      Effect.Ignore
    | `Do_nothing -> Effect.Ignore
    | `Custom_best_effort effect -> effect
  in
  Beforeunload_js_of_ocaml.create phase ~f
  |> Attr.create_hook "global-beforeunload-listener"
;;

let visibilitychange_js_of_ocaml ~phase ~f =
  Visibilitychange_js_of_ocaml.create phase ~f
  |> Attr.create_hook "global-visibilitychange-listener"
;;

module For_testing = struct
  type 'a t = 'a Capture_and_bubbling.t =
    { capture : 'a option
    ; bubbling : 'a option
    }

  let combine_capture_and_bubbling
    : ('a -> unit Ui_effect.t) t -> ('a -> unit Ui_effect.t)
    =
    fun { capture; bubbling } event ->
    match capture, bubbling with
    | None, None -> Effect.Ignore
    | Some f, None -> f event
    | None, Some g -> g event
    | Some f, Some g -> Ui_effect.Many [ f event; g event ]
  ;;

  let mousedown_type_id = Mousedown.For_testing.type_id
  let mouseup_type_id = Mouseup.For_testing.type_id
  let mousemove_type_id = Mousemove.For_testing.type_id
  let keydown_type_id = Keydown.For_testing.type_id
  let click_type_id = Click.For_testing.type_id
  let contextmenu_type_id = Contextmenu.For_testing.type_id
  let visibilitychange_type_id = Visibilitychange.For_testing.type_id
  let beforeunload_type_id = Beforeunload.For_testing.type_id
  let mousedown_type_id_js_of_ocaml = Mousedown_js_of_ocaml.For_testing.type_id
  let mouseup_type_id_js_of_ocaml = Mouseup_js_of_ocaml.For_testing.type_id
  let mousemove_type_id_js_of_ocaml = Mousemove_js_of_ocaml.For_testing.type_id
  let keydown_type_id_js_of_ocaml = Keydown_js_of_ocaml.For_testing.type_id
  let click_type_id_js_of_ocaml = Click_js_of_ocaml.For_testing.type_id
  let contextmenu_type_id_js_of_ocaml = Contextmenu_js_of_ocaml.For_testing.type_id

  let visibilitychange_type_id_js_of_ocaml =
    Visibilitychange_js_of_ocaml.For_testing.type_id
  ;;

  let beforeunload_type_id_js_of_ocaml = Beforeunload_js_of_ocaml.For_testing.type_id
end
