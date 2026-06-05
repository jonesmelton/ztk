-- deterministic fixture for the data-layer tests. writing through `notes` fires
-- the FTS triggers, so notes_fts is populated as a side effect.
insert into notes (slug, kind, title, body, entry_date, metadata)
     values ( 'hello-zet'
            , 'note'
            , 'Hello, zet'
            , 'This is the first seeded note. It exists so the TUI has something to render while the data layer is wired up.'
            , '2026-06-03'
            , '{"tags":["seed","demo"]}'
            )
          , ( 'ocaml-notes'
            , 'note'
            , 'OCaml type system'
            , 'Notes on the OCaml module system and functors.'
            , '2026-06-01'
            , '{"tags":["ocaml","demo"]}'
            )
          , ( 'morning-pages'
            , 'journal'
            , 'Morning pages'
            , 'A journal entry that also mentions OCaml in passing.'
            , '2026-06-02'
            , '{"tags":["journal"]}'
            )
          , ( 'untagged-inbox'
            , 'inbox'
            , 'Quick capture'
            , 'An inbox item with no tags and no metadata at all.'
            , '2026-05-30'
            , null
            )
          -- untitled journal entry: NULL slug and title, like most real rows
          , ( null
            , 'journal'
            , null
            , 'A journal entry with no slug and no title, only a body.'
            , '2026-05-28'
            , null
            );
