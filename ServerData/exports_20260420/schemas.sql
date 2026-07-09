-- additional_info
CREATE TABLE additional_info (
  additional_info_id INTEGER PRIMARY KEY AUTOINCREMENT,
  voter_id           TEXT NOT NULL,
  who_knows          TEXT,
  additional_text    TEXT,
  updated_by         TEXT NOT NULL,
  timestamp          TEXT NOT NULL,
  FOREIGN KEY (voter_id) REFERENCES voters(voter_id)
);

-- contacts
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

-- new_contacts
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

-- routes
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

-- sqlite_sequence
CREATE TABLE sqlite_sequence(name,seq);

-- users
CREATE TABLE users (
  username           TEXT PRIMARY KEY,
  password_hash      TEXT NOT NULL,
  role               TEXT NOT NULL,
  display_name       TEXT
);

-- voters
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

