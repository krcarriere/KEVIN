-- ══════════════════════════════════════════════════════════════════════════
-- KEVIN — SQLite schema
--
-- Observational unit per table:
--   voters        — one row per registered voter (from state voter file,
--                   enriched by the routing/donation pipeline)
--   routes        — one row per (voter_id, scenario) — a voter's route
--                   assignment for a specific canvassing plan
--   contacts      — ONE ROW PER INTERACTION. Every save from the canvass
--                   form inserts one row. "Latest support rating for
--                   voter X" = ORDER BY contact_timestamp DESC LIMIT 1.
--   additional_info — one row per note added via "Add Info" modal
--   new_contacts  — one row per person we met at the door who wasn't on
--                   the roll. Interaction data lives in contacts with the
--                   same voter_id.
--   users         — auth table, replaces Logins.R
--
-- Key conventions:
--   • voter_id is the universal join key (INTEGER)
--   • TEXT is used for fields that stay character in the app (support
--     rating '1'..'6', party code, Yes/No radio values). Easier round-trip
--     with R than fighting type coercion.
--   • Timestamps stored as 'YYYY-MM-DD HH:MM:SS' text — sorts correctly
--     lexically, parses cleanly in R, no DDMMYY-vs-MMDDYY ambiguity.
--   • Dates stored as 'YYYY-MM-DD' text, same reasons.
-- ══════════════════════════════════════════════════════════════════════════

PRAGMA foreign_keys = ON;

-- ── voters ────────────────────────────────────────────────────────────────
CREATE TABLE voters (
  voter_id                 TEXT PRIMARY KEY,
  last_name                TEXT,
  first_name               TEXT,
  middle_name              TEXT,
  full_name                TEXT,
  street_number            TEXT,
  street_name              TEXT,
  suffix_a                 TEXT,
  suffix_b                 TEXT,
  unit                     TEXT,
  unit_num                 TEXT,
  zip_code                 TEXT,
  zip4_code                TEXT,
  zero_zip_code            TEXT,
  city                     TEXT,
  state                    TEXT,
  full_address             TEXT,
  street                   TEXT,
  party_code               TEXT,
  effective_date           TEXT,
  gender                   TEXT,
  registration_date        TEXT,
  date_made_inactive       TEXT,
  state_senate_district    TEXT,
  state_rep_district       TEXT,
  precinct                 INTEGER,
  ward_council             TEXT,
  ward_district            TEXT,
  general_score            REAL,
  primary_score            REAL,
  turnout_score            REAL,
  primary_2024             TEXT,
  primary_2022             TEXT,
  primary_2020             TEXT,
  general_2024             TEXT,
  general_2022             TEXT,
  general_2020             TEXT,
  nssec_2026_voter         TEXT,
  voter_status             TEXT,
  age                      INTEGER,
  lat                      REAL,
  lng                      REAL,
  times_spoken             INTEGER DEFAULT 0,
  donate_text              TEXT,
  additional               TEXT,
  who_knows                TEXT,
  is_dtc                   INTEGER DEFAULT 0,
  may_have_moved           INTEGER DEFAULT 0
);

CREATE INDEX idx_voters_precinct       ON voters(precinct);
CREATE INDEX idx_voters_last_name      ON voters(last_name);
CREATE INDEX idx_voters_street_name    ON voters(street_name);


-- ── routes ────────────────────────────────────────────────────────────────
CREATE TABLE routes (
  voter_id           TEXT NOT NULL,
  scenario           TEXT    NOT NULL,
  precinct           INTEGER NOT NULL,
  full_address       TEXT,
  voters_at_address  INTEGER,
  route              INTEGER,
  stop_order         INTEGER,
  n_stops            INTEGER,
  dist_miles         REAL,
  total_min          REAL,
  travel_min         REAL,
  door_min           REAL,
  overhead_min       REAL,
  sheet              TEXT,
  route_name         TEXT,
  PRIMARY KEY (voter_id, scenario),
  FOREIGN KEY (voter_id) REFERENCES voters(voter_id)
);

CREATE INDEX idx_routes_scen_prec_route ON routes(scenario, precinct, route);
CREATE INDEX idx_routes_scen_prec_stop  ON routes(scenario, precinct, route, stop_order);


-- ── contacts ──────────────────────────────────────────────────────────────
CREATE TABLE contacts (
  contact_id         INTEGER PRIMARY KEY AUTOINCREMENT,
  voter_id           TEXT NOT NULL,
  contact_date       TEXT    NOT NULL,
  contact_timestamp  TEXT    NOT NULL,
  input_by           TEXT    NOT NULL,
  campaigner         TEXT,
  left_material      TEXT,
  were_they_there    TEXT,
  had_conversation   TEXT,
  support_rating     TEXT,
  concern1           TEXT,
  concern1_other     TEXT,
  concern2           TEXT,
  concern2_other     TEXT,
  concern3           TEXT,
  concern3_other     TEXT,
  notes              TEXT,
  FOREIGN KEY (voter_id) REFERENCES voters(voter_id)
);

CREATE INDEX idx_contacts_voter    ON contacts(voter_id);
CREATE INDEX idx_contacts_voter_ts ON contacts(voter_id, contact_timestamp DESC);
CREATE INDEX idx_contacts_date     ON contacts(contact_date);


-- ── additional_info ───────────────────────────────────────────────────────
CREATE TABLE additional_info (
  additional_info_id INTEGER PRIMARY KEY AUTOINCREMENT,
  voter_id           TEXT NOT NULL,
  who_knows          TEXT,
  additional_text    TEXT,
  updated_by         TEXT NOT NULL,
  timestamp          TEXT NOT NULL,
  FOREIGN KEY (voter_id) REFERENCES voters(voter_id)
);

CREATE INDEX idx_additional_info_voter ON additional_info(voter_id);


-- ── new_contacts ──────────────────────────────────────────────────────────
CREATE TABLE new_contacts (
  voter_id           TEXT PRIMARY KEY,
  first_name         TEXT,
  last_name          TEXT,
  address            TEXT,
  precinct           INTEGER,
  route              INTEGER,
  stop_order         INTEGER,
  scenario           TEXT,
  recorded_by        TEXT NOT NULL,
  timestamp          TEXT NOT NULL
);


-- ── users ─────────────────────────────────────────────────────────────────
CREATE TABLE users (
  username           TEXT PRIMARY KEY,
  password_hash      TEXT NOT NULL,
  role               TEXT NOT NULL,
  display_name       TEXT
);
