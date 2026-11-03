open! Core
open! Js_of_ocaml

module type S = Hooks_intf.S
module type Input = Hooks_intf.Input

let cancel_animation_frame id =
  let window = Browser_expert.Global.window_exn () in
  window##cancelAnimationFrame ~handle:id
;;

let request_animation_frame f =
  let window = Browser_expert.Global.window_exn () in
  let callback ~highResTime = f highResTime in
  window##requestAnimationFrame ~callback:(Js.wrap_meth_callback (fun () -> callback))
;;

module Extra = struct
  type t =
    | T :
        { type_id : 'a Type_equal.Id.t
        ; value : 'a
        }
        -> t

  let sexp_of_t (T { type_id; value }) = Type_equal.Id.to_sexp type_id value
end

type t =
  | T :
      { input : 'input
      ; input_id : 'input Type_equal.Id.t
      ; combine_inputs : 'input -> 'input -> 'input
      ; init : 'input -> Browser_types.html_element Js.t -> 'input * 'extra * 'state
      ; update :
          'input
          -> 'input * 'extra * 'state
          -> Browser_types.html_element Js.t
          -> 'input * 'extra * 'state
      ; destroy : 'input * 'extra * 'state -> Browser_types.html_element Js.t -> unit
      ; id : ('input * 'extra * 'state) Core.Type_equal.Id.t
      }
      -> t

let generic_hook = lazy Js.Unsafe.(get global (Js.string "GenericHook"))

let make_hook ~combine_inputs ~init ~extra:(input, input_id) ~update ~destroy ~id =
  T { init; combine_inputs; input; input_id; update; destroy; id }
;;

let unsafe_create = make_hook

let unsafe_create_js_of_ocaml ~combine_inputs ~init ~extra ~update ~destroy ~id =
  let init input element =
    init input (Browser_extra.Coercion.to_dom_html_element element)
  in
  let update input new_input element =
    update input new_input (Browser_extra.Coercion.to_dom_html_element element)
  in
  let destroy input element =
    destroy input (Browser_extra.Coercion.to_dom_html_element element)
  in
  make_hook ~combine_inputs ~extra ~id ~init ~update ~destroy
;;

let pack (T { init; input; input_id; update; destroy; id; _ }) =
  let wrap a = a |> Js.wrap_callback |> Js.Unsafe.inject in
  let init = wrap (init input) in
  let update = wrap (update input) in
  let destroy = wrap destroy in
  let generic_hook = Lazy.force generic_hook in
  let extra = Extra.T { type_id = input_id; value = input } in
  Js.Unsafe.fun_call
    generic_hook
    [| init; update; destroy; id |> Js.Unsafe.inject; extra |> Js.Unsafe.inject |]
;;

let combine (T left) (T right) =
  match Type_equal.Id.same_witness left.input_id right.input_id with
  | None ->
    eprint_s
      [%message
        "hooks do not have the same type, so they cannot be combined; taking the second \
         of the two"];
    T right
  | Some T -> T { right with input = right.combine_inputs left.input right.input }
;;

module Make (S : S with type html_element := Browser_types.html_element) = struct
  let input_and_state_id =
    Type_equal.Id.create ~name:"" (fun (input, _animation_id, state) ->
      [%sexp_of: S.Input.t * opaque] (input, state))
  ;;

  let input_id = Type_equal.Id.create ~name:"" S.Input.sexp_of_t

  let init input element =
    let state = S.init input element in
    let warn_if_disconnected () =
      let element : < isConnected : bool Js.t Js.prop > Js.t = Js.Unsafe.coerce element in
      if not (element##.isConnected |> Js.to_bool)
      then
        Console.console##warn_2
          (Js.string "[on_mount] hook ran, but element was not connected.")
          element
    in
    let on_destroy =
      match S.on_mount with
      | `Do_nothing -> fun () -> ()
      | `Schedule_immediately_after_this_dom_patch_completes on_mount ->
        let should_run = ref true in
        (* It is unlikely that a hook is destroyed immediately after being created, but it
           can happen, if the creation and destruction happen in the [on_mount] of a
           different hook. In this case, the element never gets displayed to the user, so
           we just don't run its [on_mount]. *)
        On_mount.Private_for_this_library_only.schedule (fun () ->
          if !should_run
          then (
            warn_if_disconnected ();
            on_mount input state element));
        fun () -> should_run := false
      | `Schedule_animation_frame on_mount ->
        let animation_id =
          request_animation_frame (fun _ ->
            warn_if_disconnected ();
            on_mount input state element)
        in
        fun () -> cancel_animation_frame animation_id
    in
    input, on_destroy, state
  ;;

  let update input (old_input, on_destroy, state) element =
    S.update ~old_input ~new_input:input state element;
    input, on_destroy, state
  ;;

  let destroy (old_input, on_destroy, state) element =
    on_destroy ();
    S.destroy old_input state element
  ;;

  let create (input : S.Input.t) =
    make_hook
      ~extra:(input, input_id)
      ~combine_inputs:S.Input.combine
      ~id:input_and_state_id
      ~init
      ~update
      ~destroy
  ;;

  module For_testing = struct
    let type_id = input_id
  end
end

module Make_js_of_ocaml (S : S with type html_element := Dom_html.element) = struct
  module S_js_of_ocaml = struct
    module State = S.State
    module Input = S.Input

    let init input element =
      S.init input (Browser_extra.Coercion.to_dom_html_element element)
    ;;

    let on_mount =
      match S.on_mount with
      | `Do_nothing -> `Do_nothing
      | `Schedule_animation_frame f ->
        let f input state (element : Browser_types.html_element Js.t) =
          f input state (Browser_extra.Coercion.to_dom_html_element element)
        in
        `Schedule_animation_frame f
      | `Schedule_immediately_after_this_dom_patch_completes f ->
        let f input state (element : Browser_types.html_element Js.t) =
          f input state (Browser_extra.Coercion.to_dom_html_element element)
        in
        `Schedule_immediately_after_this_dom_patch_completes f
    ;;

    let update ~old_input ~new_input state element =
      S.update
        ~old_input
        ~new_input
        state
        (Browser_extra.Coercion.to_dom_html_element element)
    ;;

    let destroy input state element =
      S.destroy input state (Browser_extra.Coercion.to_dom_html_element element)
    ;;
  end

  include Make (S_js_of_ocaml)
end

module For_testing = struct
  module Extra = Extra
end
