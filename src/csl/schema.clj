(ns csl.schema
  "The index schema. The index is a cache of analysis, so a schema change
  bumps `version` and the index is rebuilt rather than migrated.")

(def version
  "Bump on any change to the DDL below or to the meaning of stored values."
  3)

(def ddl
  ["CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)"

   "CREATE TABLE sym (id INTEGER PRIMARY KEY, text TEXT NOT NULL UNIQUE)"

   ;; one row per analyzed Clojure file (source file or jar entry); `key` is
   ;; the SHA-256 of every input to its analysis (csl.writer/unit-key-hash)
   "CREATE TABLE unit (
      id         INTEGER PRIMARY KEY,
      key        BLOB NOT NULL UNIQUE,
      external   INTEGER NOT NULL,
      created_at INTEGER NOT NULL)"

   ;; a jar (or the JDK's src.zip) as one unit of work
   "CREATE TABLE jar (
      id INTEGER PRIMARY KEY, jar_hash BLOB NOT NULL, config_hash BLOB NOT NULL,
      kondo_version TEXT NOT NULL, options_hash BLOB NOT NULL,
      UNIQUE (jar_hash, config_hash, kondo_version, options_hash))"
   "CREATE TABLE jar_entry (
      jar_id INTEGER NOT NULL, entry_path TEXT NOT NULL, unit_id INTEGER NOT NULL,
      PRIMARY KEY (jar_id, entry_path)) WITHOUT ROWID"
   "CREATE INDEX jar_entry_unit ON jar_entry (unit_id)"

   ;; projects and their current snapshot
   "CREATE TABLE project (
      id INTEGER PRIMARY KEY, root TEXT NOT NULL UNIQUE,
      config_hash BLOB, classpath_hash BLOB, last_seen INTEGER)"
   "CREATE TABLE project_file (
      project_id INTEGER NOT NULL, path TEXT NOT NULL,
      unit_id INTEGER,
      external INTEGER NOT NULL,
      ord INTEGER NOT NULL,
      PRIMARY KEY (project_id, path)) WITHOUT ROWID"
   "CREATE INDEX project_file_unit ON project_file (unit_id)"
   "CREATE TABLE project_jar (
      project_id INTEGER NOT NULL, ord INTEGER NOT NULL, path TEXT NOT NULL,
      jar_id INTEGER,
      PRIMARY KEY (project_id, ord)) WITHOUT ROWID"
   "CREATE INDEX project_jar_jar ON project_jar (jar_id)"

   ;; library files someone opened (extracted by csl.sources), fully
   ;; analyzed; not part of any project, alive while their jar exists
   "CREATE TABLE dep_file (
      path TEXT PRIMARY KEY, jar_hash BLOB NOT NULL, unit_id INTEGER NOT NULL) WITHOUT ROWID"

   ;; derived: every unit visible to a project, with its precedence
   "CREATE TABLE project_unit (
      project_id INTEGER NOT NULL, unit_id INTEGER NOT NULL,
      ord INTEGER NOT NULL,
      PRIMARY KEY (project_id, unit_id)) WITHOUT ROWID"
   "CREATE INDEX project_unit_unit ON project_unit (unit_id)"

   ;; memo tables
   "CREATE TABLE fingerprint (
      path TEXT PRIMARY KEY, mtime INTEGER NOT NULL, size INTEGER NOT NULL,
      content_hash BLOB NOT NULL) WITHOUT ROWID"
   "CREATE TABLE classpath_memo (
      project_id INTEGER NOT NULL, spec_hash BLOB NOT NULL, classpath TEXT NOT NULL,
      PRIMARY KEY (project_id, spec_hash)) WITHOUT ROWID"

   ;; analysis rows: ns/name columns are sym ids, 0 = none; lang is a bit mask
   "CREATE TABLE definition (
      id INTEGER PRIMARY KEY, unit_id INTEGER NOT NULL,
      kind INTEGER NOT NULL,
      ns INTEGER NOT NULL, name INTEGER NOT NULL, lang INTEGER NOT NULL,
      name_row INTEGER, name_col INTEGER, name_end_row INTEGER, name_end_col INTEGER,
      flags INTEGER NOT NULL DEFAULT 0,
      defined_by INTEGER, defined_by_lint_as INTEGER,
      extra TEXT)"
   "CREATE INDEX definition_ns_name ON definition (ns, name)"
   "CREATE INDEX definition_unit ON definition (unit_id)"

   "CREATE TABLE usage (
      to_ns INTEGER NOT NULL, name INTEGER NOT NULL, unit_id INTEGER NOT NULL,
      name_row INTEGER NOT NULL, name_col INTEGER NOT NULL,
      lang INTEGER NOT NULL,
      kind INTEGER NOT NULL,
      name_end_row INTEGER, name_end_col INTEGER,
      from_ns INTEGER, from_var INTEGER,
      flags INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (to_ns, name, unit_id, name_row, name_col, lang, kind)) WITHOUT ROWID"
   ;; no unit_id index: GC finds a dead unit's usage rows through its
   ;; file_element rows, which hold every column of the usage key

   "CREATE TABLE doc (definition_id INTEGER PRIMARY KEY, docstring TEXT)"

   "CREATE TABLE file_element (
      unit_id INTEGER NOT NULL, name_row INTEGER NOT NULL, name_col INTEGER NOT NULL,
      kind INTEGER NOT NULL,
      lang INTEGER NOT NULL,
      name_end_row INTEGER, name_end_col INTEGER,
      ns INTEGER, name INTEGER, alias INTEGER,
      local_id INTEGER,
      form_row INTEGER, form_col INTEGER,
      form_end_row INTEGER, form_end_col INTEGER,
      PRIMARY KEY (unit_id, name_row, name_col, kind, lang)) WITHOUT ROWID"

   ;; Java classes are recorded per jar, not as units: one unit per .class
   ;; file cost a unit, jar_entry and project_unit row each (Phase 1)
   "CREATE TABLE java_class (
      name INTEGER NOT NULL,
      jar_id INTEGER NOT NULL,
      entry_path TEXT NOT NULL,
      PRIMARY KEY (name, jar_id)) WITHOUT ROWID"
   "CREATE INDEX java_class_jar ON java_class (jar_id)"

   "CREATE VIRTUAL TABLE name_fts USING fts5(text, content='', contentless_delete=1, tokenize='trigram')"

   ;; work queue: written by lsp processes, drained by the daemon
   "CREATE TABLE pending (
      project_id INTEGER NOT NULL,
      kind TEXT NOT NULL,
      path TEXT NOT NULL,
      priority INTEGER NOT NULL,
      enqueued_at INTEGER NOT NULL,
      PRIMARY KEY (project_id, kind, path)) WITHOUT ROWID"

   ;; daemon registry; liveness is the daemon.lock file lock, not this row
   "CREATE TABLE daemon (
      id INTEGER PRIMARY KEY CHECK (id = 1), pid INTEGER, version TEXT,
      started_at INTEGER, heartbeat_at INTEGER,
      stop_requested INTEGER NOT NULL DEFAULT 0)"])
