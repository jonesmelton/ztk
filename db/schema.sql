-- zet schema (authoritative; mirrors docs/decisions.md).
-- Single-writer assumption: plain connection, no WAL gymnastics.

CREATE TABLE IF NOT EXISTS notes (
  id          INTEGER PRIMARY KEY,
  slug        TEXT UNIQUE NOT NULL,
  kind        TEXT NOT NULL CHECK (kind IN ('journal','note','inbox')),
  title       TEXT NOT NULL,
  body        TEXT NOT NULL,
  entry_date  TEXT,
  metadata    TEXT,            -- JSON, e.g. {"tags":[...]}
  created_at  TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS assets (
  id          INTEGER PRIMARY KEY,
  filename    TEXT UNIQUE NOT NULL,
  mime_type   TEXT,
  size_bytes  INTEGER,
  note_id     INTEGER REFERENCES notes(id),
  created_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS note_assets (
  note_id   INTEGER NOT NULL REFERENCES notes(id),
  asset_id  INTEGER NOT NULL REFERENCES assets(id),
  PRIMARY KEY (note_id, asset_id)
);

-- FTS5 external-content index over notes(title, body), kept in sync by triggers.
CREATE VIRTUAL TABLE IF NOT EXISTS notes_fts USING fts5 (
  title,
  body,
  content='notes',
  content_rowid='id'
);

CREATE TRIGGER IF NOT EXISTS notes_ai AFTER INSERT ON notes BEGIN
  INSERT INTO notes_fts (rowid, title, body) VALUES (new.id, new.title, new.body);
END;

CREATE TRIGGER IF NOT EXISTS notes_ad AFTER DELETE ON notes BEGIN
  INSERT INTO notes_fts (notes_fts, rowid, title, body)
    VALUES ('delete', old.id, old.title, old.body);
END;

CREATE TRIGGER IF NOT EXISTS notes_au AFTER UPDATE ON notes BEGIN
  INSERT INTO notes_fts (notes_fts, rowid, title, body)
    VALUES ('delete', old.id, old.title, old.body);
  INSERT INTO notes_fts (rowid, title, body) VALUES (new.id, new.title, new.body);
END;
