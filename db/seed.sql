-- One test note. Writing through `notes` fires the FTS triggers.
INSERT INTO notes (slug, kind, title, body, entry_date, metadata)
VALUES (
  'hello-zet',
  'note',
  'Hello, zet',
  'This is the first seeded note. It exists so the TUI has something to render while the data layer is wired up.',
  '2026-06-03',
  '{"tags":["seed","demo"]}'
);
