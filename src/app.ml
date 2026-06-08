open! Core
open! Bonsai_term
open Bonsai.Let_syntax
module Focus = Model.Focus
module Mode = Model.Mode
module Model_state = Model.Model

let route = Model.route
let apply_action_pure = Model.apply_action_pure

let app ~(db : Db.t) ~(dimensions : Dimensions.t Bonsai.t) (local_ graph)
  : view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  =
  (* Browse corpus, held as Bonsai state so an edit can refresh it without restarting. The
     list value is its own immutable box: [reload_notes] re-pulls a *fresh* list, so the
     phys_equal cutoff sees the change (see the virtual_list perf note in CLAUDE.md). The
     live handle stays open so both search and reload can re-query; see [launch_tui]. *)
  (* No [~equal]: the cutoff is [phys_equal] by design. A reloaded corpus is a freshly
     allocated list, so identity differs and dependents recompute; an unchanged corpus
     keeps the same value and is cut off. *)
  let notes, set_notes = Bonsai.state' (Db.list_all db) graph in
  let reload_notes =
    let%arr set_notes in
    set_notes (fun _ -> Db.list_all db)
  in
  let model, set_model =
    Bonsai.state'
      Model_state.initial
      ~sexp_of_model:[%sexp_of: Model_state.t]
      ~equal:[%equal: Model_state.t]
      graph
  in
  (* Live FTS results, derived from the query buffer. The raw buffer is sanitized to a
     safe MATCH (so half-typed input can't raise an FTS5 error), then [cutoff] on the
     string keeps us from re-querying when the buffer changes in a way that doesn't change
     the query text. An empty sanitized query yields no results (rather than a [""]
     MATCH). *)
  let safe_query =
    Bonsai.cutoff
      ~equal:String.equal
      (let%arr model in
       Fts_query.sanitize model.editor.buf)
  in
  let results =
    let%arr safe_query in
    if String.is_empty safe_query
    then []
    else Db.search db ~query:safe_query ~limit:200 ()
  in
  (* The active list backs both selection and rendering: results in Search, corpus in
     Browse. Cursor/scroll clamping is computed against whichever is active. *)
  let active =
    let%arr model and results and notes in
    match model.mode with
    | Search -> results
    | Browse | Help | Edit -> notes
  in
  (* Border boxes cost 2 cols each (left+right); two boxes = 4. Split the rest
     golden-ratio: list ~38%, detail ~62%. *)
  let panes =
    let%arr { Dimensions.width; height } = dimensions in
    let content_width = width - 4 in
    let list_w = content_width * 382 / 1000 |> Int.max 10 in
    let detail_w = Int.max 10 (content_width - list_w) in
    let pane_h = Int.max 3 (height - 2) in
    list_w, detail_w, pane_h
  in
  (* The text editor component. Instantiated unconditionally (Bonsai graph nodes are
     static): it always exists, but is only rendered and fed keys in [Edit] mode. On entry
     we seed it with the note body via [set_text]; on save we read [editor_text]. Sized to
     the detail pane's interior. *)
  let editor_width =
    let%arr _, detail_w, _ = panes in
    Int.max 1 (detail_w - 2)
  in
  let editor_max_height =
    let%arr _, _, pane_h = panes in
    Int.max 1 pane_h
  in
  let%tydi { text = editor_text
           ; send_actions = editor_send_actions
           ; view = editor_view
           ; cursor = editor_cursor
           ; set_text = editor_set_text
           ; rope = _
           ; get_cursor_position = editor_get_cursor_position
           }
    =
    Bonsai_term_text_editor.component
      ~text_attrs:(Bonsai.return [])
      ~width:editor_width
      ~max_height:editor_max_height
      graph
  in
  let editor_handler =
    Bonsai_term_text_editor.Emacs.emacs_keybindings_handler editor_send_actions graph
  in
  let editor_handler =
    Bonsai_term_text_editor.Buffer_and_apply_paste_events_in_bulk.f
      ~send_actions:editor_send_actions
      ~handler:editor_handler
      graph
  in
  (* The [C-x C-s] save chord lives in its own state machine so its armed-bit is read and
     updated inside [apply_action] — against the freshly-applied model — rather than from
     a handler closure that's only as fresh as the last frame. That matters because the
     bonsai_term run loop can deliver several keystrokes from one input read and dispatch
     them all through a single frame's handler (see driver.ml): a [C-x] then [C-s]
     arriving in the same batch would, in a closure that captured [pending = false], miss
     the chord. Here each action sees the prior action's result, so batching is safe.

     Input carries what the effects need: the id being edited (pins the save target), the
     live editor text (the new body), and the two effects to run on completion — refresh
     the corpus and drop back to Browse. *)
  let to_browse =
    let%arr set_model in
    set_model (fun (m : Model_state.t) ->
      { m with mode = Browse; editing_id = None; mark = None })
  in
  let chord_input =
    let%arr model and editor_text and reload_notes and to_browse in
    model.editing_id, editor_text, reload_notes, to_browse
  in
  let module Chord = struct
    type t =
      | Arm (* [C-x]: arm the chord *)
      | Disarm (* any non-chord key: cancel a half-typed [C-x] without leaving Edit *)
      | Save (* [C-s]: if armed, persist + leave Edit *)
      | Cancel (* [C-g]: leave Edit without saving *)
    [@@deriving sexp_of]
  end
  in
  let _pending, inject_chord =
    Bonsai.state_machine_with_input
      ~default_model:false
      ~sexp_of_model:[%sexp_of: bool]
      ~sexp_of_action:[%sexp_of: Chord.t]
      ~apply_action:(fun ctx input armed (action : Chord.t) ->
        match input with
        | Bonsai.Computation_status.Inactive -> armed
        | Active (editing_id, editor_text, reload_notes, to_browse) ->
          (match action with
           | Arm -> true
           | Disarm -> false
           | Cancel ->
             Bonsai.Apply_action_context.schedule_event ctx to_browse;
             false
           | Save ->
             (* Only an armed [C-s] saves; a bare [C-s] is inert. *)
             (match armed, editing_id with
              | true, Some id ->
                Db.update_body db ~id ~body:editor_text;
                Bonsai.Apply_action_context.schedule_event
                  ctx
                  (Effect.all_unit [ reload_notes; to_browse ])
              | _ -> ());
             false))
      chord_input
      graph
  in
  (* A single inject takes the raw key and does mode/focus routing inside [set_model], so
     it reads the live model rather than a handler value that's only as fresh as the last
     view recompute. [count]/[detail_max] for nav clamping come from the *active* list and
     live pane geometry; a one-frame-stale [active] only affects nav clamping (which
     self-corrects next frame), never query editing. *)
  let inject =
    let%arr set_model
    and active
    and _, detail_w, pane_h = panes in
    fun (event : Event.t) ->
      set_model (fun model ->
        match route model event with
        | None -> model
        | Some action ->
          let count = List.length active in
          let selected = List.nth active model.cursor in
          let detail_max =
            Render.detail_max_scroll ~width:detail_w ~height:pane_h selected
          in
          apply_action_pure ~count ~detail_max model action)
  in
  let view =
    let%arr model
    and active
    and editor_view
    and { Dimensions.width; height } = dimensions
    and list_w, detail_w, pane_h = panes in
    let editing = [%equal: Mode.t] model.mode Edit in
    let selected = List.nth active model.cursor in
    let list_focused = [%equal: Focus.t] model.focus List in
    let searching = [%equal: Mode.t] model.mode Search in
    (* Terms to emphasize: the query words, lowercased, only while searching. Empty in
       browse, so [highlight] is a no-op there. *)
    let terms =
      if searching
      then Fts_query.tokens model.editor.buf |> List.map ~f:String.lowercase
      else []
    in
    (* The border box lays [title] into the top edge as-is (no truncation), so callers
       must size it to the pane — [list_title ~budget] does that for the search query. The
       [╭ ] prefix and trailing [ ─╮] consume ~4 cols; the [<tab>] hint (shown on the
       unfocused pane) eats more, so its width is folded into the budget below. *)
    let box ~focused ~title content =
      let color = if focused then Render.accent else Render.dim in
      let tab = if focused then "" else " <tab>" in
      Bonsai_term_border_box.view
        ~line_type:Round_corners
        ~attrs:[ Attr.fg color ]
        ~title:[%string "%{title}%{tab}"]
        ~title_attrs:[ Attr.fg color; Attr.bold ]
        content
    in
    let tab_cols = if list_focused then 0 else String.length " <tab>" in
    let list_box =
      box
        ~focused:list_focused
        ~title:(Render.list_title ~budget:(list_w - 4 - tab_cols) model)
        (Render.render_list
           ~empty_label:(if searching then "(no matches)" else "(no notes)")
           ~terms
           ~width:list_w
           ~height:pane_h
           ~cursor:model.cursor
           active)
    in
    let detail_box =
      if editing
      then (
        (* Editing takes over the detail pane: the editor's own view replaces the
           read-only body, the box is focused, and the title shows the save/cancel chord.
           Pinned to the pane geometry so the frame doesn't resize to the text. *)
        let title =
          match model.mark with
          | Some n -> [%string "Edit  mark@%{n#Int}  M-w copy  C-g clear"]
          | None -> "Edit  C-x C-s save  C-g cancel"
        in
        box ~focused:true ~title (Render.fit ~width:detail_w ~height:pane_h editor_view))
      else
        box
          ~focused:(not list_focused)
          ~title:"Detail"
          (Render.render_detail
             ~terms
             ~width:detail_w
             ~height:pane_h
             ~scroll:model.detail_scroll
             selected)
    in
    let content = View.hcat [ list_box; detail_box ] in
    (* In Help mode the cheat-sheet sits on top of the panes, which stay visible behind
       it. [zcat] draws earlier elements on top, so the overlay goes first. *)
    let overlay =
      match model.mode with
      | Help -> [ Render.render_help ~width ~height ]
      | Browse | Search | Edit -> []
    in
    (* Backdrop so the framed panes sit on a full-screen rectangle. *)
    View.zcat (overlay @ [ content; View.rectangle ~width ~height () ])
  in
  (* The handler routes by mode. Browse/Search/Help go through [inject] (the pure
     reducer). Edit is split: the save/cancel chord goes to [inject_chord] (the batch-safe
     state machine), and every other key is delegated to the text editor's own handler.
     Opening the editor ([e]) seeds it with the note body and flips mode; that's a single
     key, so routing it through [set_model] is fine. *)
  let handler =
    let%arr inject
    and set_model
    and model
    and active
    and editor_handler
    and editor_set_text
    and editor_send_actions
    and editor_cursor
    and editor_text
    and inject_chord in
    fun (event : Event.t) ->
      let discard eff = Effect.map eff ~f:(fun (_ : Captured_or_ignored.t) -> ()) in
      match model.mode with
      | Browse | Search | Help ->
        (* [e] on a selected note opens the editor seeded with its body. Everything else
           in these modes is the reducer's job. *)
        (match event, model.mode with
         | Key_press { key = ASCII 'e'; mods = [] }, Browse ->
           (match List.nth active model.cursor with
            | None -> Effect.return ()
            | Some (note : Db.Note.t) ->
              Effect.all_unit
                [ editor_set_text note.body
                ; set_model (fun (m : Model_state.t) ->
                    { m with mode = Edit; editing_id = Some note.id })
                ])
         | _ -> inject event)
      | Edit ->
        (match event with
         (* [C-g] clears the mark if one is set (without leaving Edit); otherwise it
            cancels the edit. So the same key both un-arms a region and backs out, in that
            order. *)
         | Key_press { key = ASCII 'G'; mods = [ Ctrl ] } ->
           (match model.mark with
            | Some _ -> set_model (fun (m : Model_state.t) -> { m with mark = None })
            | None -> inject_chord Cancel)
         | Key_press { key = ASCII 'X'; mods = [ Ctrl ] } -> inject_chord Arm
         | Key_press { key = ASCII 'S'; mods = [ Ctrl ] } -> inject_chord Save
         (* [C-Space] sets the mark at the editor's caret. Terminals deliver Ctrl+Space as
            control code 0x00, which the input parser canonicalizes to [C-@] (they share
            the code); some emit [ASCII ' '] instead, so accept both. *)
         | Key_press { key = ASCII ('@' | ' '); mods = [ Ctrl ] } ->
           set_model (fun (m : Model_state.t) ->
             { m with mark = Some editor_cursor.position })
         (* [M-w] copies the region [mark, caret] into the kill ring and clears the mark
            (Emacs semantics). No mark = no-op. [position] is a codepoint offset, so slice
            the UTF-8 buffer with [Zed_utf8.sub] to stay multibyte-correct. *)
         | Key_press { key = ASCII 'w'; mods = [ Meta ] } ->
           (match model.mark with
            | None -> Effect.return ()
            | Some m ->
              let lo = Int.min m editor_cursor.position in
              let hi = Int.max m editor_cursor.position in
              let region = Zed.Zed_utf8.sub editor_text lo (hi - lo) in
              set_model (fun (model : Model_state.t) ->
                { model with kill_ring = Some region; mark = None }))
         (* [C-y] yanks our kill ring at the caret via the editor's [Insert]. Intercepted
            before delegation so it inserts *our* ring, not the editor's internal one
            (which its Emacs handler binds to [C-y] / [Yank]). Empty ring = no-op. *)
         | Key_press { key = ASCII 'Y'; mods = [ Ctrl ] } ->
           (match model.kill_ring with
            | None | Some "" -> Effect.return ()
            | Some text -> editor_send_actions [ Insert text ])
         (* Any other key disarms a half-typed chord and goes to the editor. Both run: the
            disarm is a no-op when not armed. *)
         | _ -> Effect.all_unit [ inject_chord Disarm; discard (editor_handler event) ])
  in
  (* Track the terminal cursor to the editor's caret while editing; clear it otherwise so
     the block cursor doesn't linger over the read-only panes. [get_cursor_position]
     returns coordinates relative to the *whole* view it's given, so we pass the composed
     app [view] (which embeds [editor_view]) — passing the bare editor view would yield
     editor-local coords and place the cursor at the screen's top-left. Runs after each
     frame so it sees the just-rendered view. *)
  let () =
    let update_cursor =
      let%arr set_cursor = Effect.set_cursor graph
      and model
      and view
      and editor_get_cursor_position in
      match model.mode with
      | Browse | Search | Help -> set_cursor None
      | Edit ->
        (match editor_get_cursor_position view with
         | None -> set_cursor None
         | Some ({ x; y } : Position.t) ->
           set_cursor (Some { position = { x; y }; kind = Bar_blinking }))
    in
    Bonsai.Edge.after_display update_cursor graph
  in
  ~view, ~handler
;;
