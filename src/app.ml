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
  (* No [~equal]: phys_equal by design. A reloaded corpus is a freshly allocated list so
     dependents recompute; an unchanged corpus keeps the same pointer and is cut off. *)
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
  (* Cutoff on the sanitized string avoids re-querying when editing the buffer doesn't
     change the actual MATCH expression (e.g. trailing space). *)
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
  let active =
    let%arr model and results and notes in
    match model.mode with
    | Search -> results
    | Browse | Help | Edit | Extract -> notes
  in
  (* Golden-ratio split: list ~38%, detail ~62%. Border boxes cost 4 cols total. *)
  let panes =
    let%arr { Dimensions.width; height } = dimensions in
    let content_width = width - 4 in
    let list_w = content_width * 382 / 1000 |> Int.max 10 in
    let detail_w = Int.max 10 (content_width - list_w) in
    let pane_h = Int.max 3 (height - 2) in
    list_w, detail_w, pane_h
  in
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
  (* Discovery search for Extract mode: the typed title lives in the text-editor component
     ([editor_text]), not [model.editor.buf], so it needs its own query. Same FTS index as
     [search] (title + body), so a related note surfaces even when its title differs from
     what you are typing. Purely informational — it never drives the append/create fork,
     which keys off the exact slug match in the Save handler. *)
  let extract_query =
    Bonsai.cutoff
      ~equal:String.equal
      (let%arr editor_text and model in
       match model.mode with
       | Extract -> Fts_query.sanitize editor_text
       | Browse | Search | Help | Edit -> "")
  in
  let extract_results =
    let%arr extract_query in
    if String.is_empty extract_query
    then []
    else Db.search db ~query:extract_query ~limit:200 ()
  in
  (* The C-x C-s chord lives in a state_machine so its armed-bit is updated inside
     apply_action. The run loop can batch multiple keystrokes into one frame; a handler
     closure would see a stale [pending=false] for both keys. The state machine sees each
     action's result in sequence, so C-x then C-s in the same batch still fires the save. *)
  let to_browse =
    let%arr set_model in
    set_model (fun (m : Model_state.t) ->
      { m with mode = Browse; editing_id = None; mark = None; extract = None })
  in
  (* Return to Browse with the given note id selected and the detail pane focused. Used by
     the append-to-existing extract path to land on the note we just appended to. The
     cursor is an index into the active corpus, so we look the id up in a fresh [list_all]
     (the same ordering Browse uses); if it is missing the cursor falls back to 0. *)
  let open_note =
    let%arr set_model in
    fun id ->
      set_model (fun (m : Model_state.t) ->
        let cursor =
          List.findi (Db.list_all db) ~f:(fun _ (n : Db.Note.t) -> n.id = id)
          |> Option.value_map ~default:0 ~f:fst
        in
        { m with
          mode = Browse
        ; focus = Detail
        ; cursor
        ; detail_scroll = 0
        ; editing_id = None
        ; mark = None
        ; extract = None
        })
  in
  let module Chord_input = struct
    type t =
      { model : Model_state.t
      ; editor_text : string
      ; editor_position : int
      ; reload_notes : unit Effect.t
      ; to_browse : unit Effect.t
      ; open_note : int -> unit Effect.t
      ; set_model : (Model_state.t -> Model_state.t) -> unit Effect.t
      ; editor_set_text : string -> unit Effect.t
      ; editor_send_actions :
          Bonsai_term_text_editor.Action.t Nonempty_list.t -> unit Effect.t
      }
  end
  in
  let chord_input =
    let%arr model
    and editor_text
    and editor_cursor
    and reload_notes
    and to_browse
    and open_note
    and set_model
    and editor_set_text
    and editor_send_actions in
    { Chord_input.model
    ; editor_text
    ; editor_position = editor_cursor.position
    ; reload_notes
    ; to_browse
    ; open_note
    ; set_model
    ; editor_set_text
    ; editor_send_actions
    }
  in
  let module Chord = struct
    type t =
      | Arm
      | Disarm (* any non-chord key: cancel a half-typed [C-x] without leaving Edit *)
      | Save
      | Cancel
      | Begin_extract
        (* [C-e] (armed, in Edit, mark set): slice the region out of the body and enter
           Extract mode to name the new note. *)
    [@@deriving sexp_of]
  end
  in
  let _chord_armed, inject_chord =
    Bonsai.state_machine_with_input
      ~default_model:false
      ~sexp_of_model:[%sexp_of: bool]
      ~sexp_of_action:[%sexp_of: Chord.t]
      ~apply_action:(fun ctx input armed (action : Chord.t) ->
        match input with
        | Bonsai.Computation_status.Inactive -> armed
        | Active
            { Chord_input.model
            ; editor_text
            ; editor_position
            ; reload_notes
            ; to_browse
            ; open_note
            ; set_model
            ; editor_set_text
            ; editor_send_actions
            } ->
          (match action with
           | Arm -> true
           | Disarm -> false
           | Cancel ->
             Bonsai.Apply_action_context.schedule_event ctx to_browse;
             false
           | Save ->
             (* Only an armed [C-s] saves; a bare [C-s] is inert. The save target depends
                on the mode: Edit overwrites the body, Extract commits the atomic split. *)
             (match armed, model.mode, model.extract, model.editing_id with
              | true, Edit, _, Some id ->
                Db.update_body db ~id ~body:editor_text;
                Bonsai.Apply_action_context.schedule_event
                  ctx
                  (Effect.all_unit [ reload_notes; to_browse ])
              | true, Extract, Some ex, _ ->
                (* [editor_text] holds the typed title. If its slug matches an existing
                   note we append the slice to that note and open it; otherwise we create
                   a new note. The slug carries the append/create decision because slug is
                   UNIQUE in the schema, so the lookup returns at most one target — no
                   ambiguity, and a slug that would have collided on create now appends
                   instead of raising. Empty title = untitled (NULL), which has no slug
                   and so always creates. *)
                let title =
                  match String.strip editor_text with
                  | "" -> None
                  | t -> Some t
                in
                let slug = Option.bind title ~f:Model.slug_of_title in
                let target = Option.bind slug ~f:(fun slug -> Db.get_by_slug db slug) in
                let after =
                  match target with
                  | Some (existing : Db.Note.t) ->
                    Db.append_region
                      db
                      ~source_id:ex.source_id
                      ~source_body:ex.source_body
                      ~target_id:existing.id
                      ~slice:ex.new_body;
                    Effect.all_unit [ reload_notes; open_note existing.id ]
                  | None ->
                    let (_ : int) =
                      Db.extract_region
                        db
                        ~source_id:ex.source_id
                        ~source_body:ex.source_body
                        ~new_slug:slug
                        ~new_kind:"note"
                        ~new_title:title
                        ~new_body:ex.new_body
                        ~new_entry_date:None
                        ~new_metadata:None
                    in
                    Effect.all_unit [ reload_notes; to_browse ]
                in
                Bonsai.Apply_action_context.schedule_event ctx after
              | _ -> ());
             false
           | Begin_extract ->
             (* [C-e] routes here unconditionally (gating on the Bonsai [armed] value
                would be stale when [C-x][C-e] batch into one frame). Only an armed [C-e]
                over a mark extracts; otherwise this is a plain editor end-of-line, which
                we must still perform since the editor never saw the key. *)
             (match armed, model.mode, model.mark, model.editing_id with
              | true, Edit, Some mark, Some source_id ->
                let lo = Int.min mark editor_position in
                let hi = Int.max mark editor_position in
                let new_body, source_body = Model.split_region ~buf:editor_text ~lo ~hi in
                Bonsai.Apply_action_context.schedule_event
                  ctx
                  (Effect.all_unit
                     [ editor_set_text ""
                     ; set_model (fun (m : Model_state.t) ->
                         { m with
                           mode = Extract
                         ; mark = None
                         ; extract =
                             Some { Model.Extract.source_id; source_body; new_body }
                         })
                     ])
              | _ ->
                Bonsai.Apply_action_context.schedule_event
                  ctx
                  (editor_send_actions [ Goto_eol ]));
             false))
      chord_input
      graph
  in
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
    and extract_results
    and editor_view
    and { Dimensions.width; height } = dimensions
    and list_w, detail_w, pane_h = panes in
    let extracting = [%equal: Mode.t] model.mode Extract in
    let editing = [%equal: Mode.t] model.mode Edit || extracting in
    (* In Extract mode the list pane shows discovery matches for the typed title, not the
       browse corpus; there is no cursor over them, so nothing is highlighted. *)
    let list_items = if extracting then extract_results else active in
    let selected = List.nth active model.cursor in
    let list_focused = [%equal: Focus.t] model.focus List in
    let searching = [%equal: Mode.t] model.mode Search in
    let terms =
      if searching
      then Fts_query.tokens model.editor.buf |> List.map ~f:String.lowercase
      else []
    in
    (* [show_tab] appends the " <tab>" focus-cycle hint. Suppressed in Extract mode, where
       Tab does not cycle panes (the editor owns the keyboard). *)
    let box ?(show_tab = true) ~focused ~title content =
      let color = if focused then Render.accent else Render.dim in
      let tab = if focused || not show_tab then "" else " <tab>" in
      Bonsai_term_border_box.view
        ~line_type:Round_corners
        ~attrs:[ Attr.fg color ]
        ~title:[%string "%{title}%{tab}"]
        ~title_attrs:[ Attr.fg color; Attr.bold ]
        content
    in
    let tab_cols = if list_focused || extracting then 0 else String.length " <tab>" in
    let list_box =
      let empty_label =
        if extracting
        then "(no related notes)"
        else if searching
        then "(no matches)"
        else "(no notes)"
      in
      (* No selection in the discovery list, so pass an out-of-range cursor to highlight
         nothing. *)
      let list_cursor = if extracting then -1 else model.cursor in
      let list_title =
        if extracting
        then "Related"
        else Render.list_title ~budget:(list_w - 4 - tab_cols) model
      in
      box
        ~show_tab:(not extracting)
        ~focused:(list_focused && not extracting)
        ~title:list_title
        (Render.render_list
           ~empty_label
           ~terms
           ~width:list_w
           ~height:pane_h
           ~cursor:list_cursor
           list_items)
    in
    let detail_box =
      if editing
      then (
        let title =
          match model.mode with
          | Extract -> "Extract  C-x C-s save  C-g cancel"
          | _ ->
            (match model.mark with
             | Some n -> [%string "Edit  mark@%{n#Int}  C-x C-e extract  M-w copy"]
             | None -> "Edit  C-x C-s save  C-g cancel")
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
    let overlay =
      match model.mode with
      | Help -> [ Render.render_help ~width ~height ]
      | Browse | Search | Edit | Extract -> []
    in
    View.zcat (overlay @ [ content; View.rectangle ~width ~height () ])
  in
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
    and reload_notes
    and inject_chord in
    fun (event : Event.t) ->
      let discard eff = Effect.map eff ~f:(fun (_ : Captured_or_ignored.t) -> ()) in
      match model.mode with
      | Browse | Search | Help ->
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
         (* [d] soft-deletes the selected note: it vanishes from the corpus on reload but
            the row survives (only [zet sweep] removes it). Reversible via [zet restore],
            since a deleted note is no longer in the active corpus for the TUI to select.
            The cursor is left as-is and clamps on the next motion. *)
         | Key_press { key = ASCII 'd'; mods = [] }, Browse ->
           (match List.nth active model.cursor with
            | None -> Effect.return ()
            | Some (note : Db.Note.t) ->
              Db.set_deleted db ~id:note.id ~deleted:true;
              reload_notes)
         | _ -> inject event)
      | Edit ->
        (match event with
         | Key_press { key = ASCII 'G'; mods = [ Ctrl ] } ->
           (match model.mark with
            | Some _ -> set_model (fun (m : Model_state.t) -> { m with mark = None })
            | None -> inject_chord Cancel)
         | Key_press { key = ASCII 'X'; mods = [ Ctrl ] } -> inject_chord Arm
         | Key_press { key = ASCII 'S'; mods = [ Ctrl ] } -> inject_chord Save
         (* [C-e] always routes to the chord machine, which decides in-sequence whether
            the chord is armed: armed over a mark extracts; otherwise it replays
            end-of-line on the editor. Gating here on the Bonsai [chord_armed] value would
            be stale when [C-x][C-e] arrive in the same frame. *)
         | Key_press { key = ASCII 'E'; mods = [ Ctrl ] } -> inject_chord Begin_extract
         (* Terminals deliver C-Space as 0x00, canonicalized to C-@; some emit ASCII ' '. *)
         | Key_press { key = ASCII ('@' | ' '); mods = [ Ctrl ] } ->
           set_model (fun (m : Model_state.t) ->
             { m with mark = Some editor_cursor.position })
         (* [position] is a codepoint offset; use [Zed_utf8.sub] to slice
            multibyte-correctly. *)
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
      | Extract ->
        (match event with
         | Key_press { key = ASCII 'G'; mods = [ Ctrl ] } -> inject_chord Cancel
         | Key_press { key = ASCII 'X'; mods = [ Ctrl ] } -> inject_chord Arm
         | Key_press { key = ASCII 'S'; mods = [ Ctrl ] } -> inject_chord Save
         | _ -> Effect.all_unit [ inject_chord Disarm; discard (editor_handler event) ])
  in
  (* Pass the composed app [view], not the bare [editor_view]: [get_cursor_position]
     returns coords relative to whatever view you give it, so passing editor_view would
     yield editor-local coords and place the cursor at the screen's top-left. *)
  let () =
    let update_cursor =
      let%arr set_cursor = Effect.set_cursor graph
      and model
      and view
      and editor_get_cursor_position in
      match model.mode with
      | Browse | Search | Help -> set_cursor None
      | Edit | Extract ->
        (match editor_get_cursor_position view with
         | None -> set_cursor None
         | Some ({ x; y } : Position.t) ->
           set_cursor (Some { position = { x; y }; kind = Bar_blinking }))
    in
    Bonsai.Edge.after_display update_cursor graph
  in
  ~view, ~handler
;;
