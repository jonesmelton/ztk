-- zet schema (authoritative; mirrors docs/decisions.md).
-- single-writer assumption: plain connection, no WAL gymnastics.

create table if not exists notes (
  id          integer primary key,
  slug        text unique,     -- nullable: untitled journal entries have none
  kind        text not null check (kind in ('journal','note','inbox')),
  title       text,            -- nullable: untitled journal entries have none
  body        text not null,
  entry_date  text,
  metadata    text,            -- JSON, e.g. {"tags":[...]}
  created_at  text not null default (datetime('now')),
  updated_at  text not null default (datetime('now'))
);

create table if not exists assets (
  id          integer primary key,
  filename    text unique not null,
  mime_type   text,
  size_bytes  integer,
  note_id     integer references notes(id),
  created_at  text not null default (datetime('now'))
);

create table if not exists note_assets (
  note_id   integer not null references notes(id),
  asset_id  integer not null references assets(id),
  primary key (note_id, asset_id)
);

-- FTS5 external-content index over notes(title, body), kept in sync by triggers.
create virtual table if not exists notes_fts using fts5 (
  title,
  body,
  content='notes',
  content_rowid='id'
);

create trigger if not exists notes_ai after insert on notes begin
  insert into notes_fts (rowid, title, body) values (new.id, new.title, new.body);
end;

create trigger if not exists notes_ad after delete on notes begin
  insert into notes_fts (notes_fts, rowid, title, body)
       values ('delete', old.id, old.title, old.body);
end;

create trigger if not exists notes_au after update on notes begin
  insert into notes_fts (notes_fts, rowid, title, body)
       values ('delete', old.id, old.title, old.body);
  insert into notes_fts (rowid, title, body) values (new.id, new.title, new.body);
end;
