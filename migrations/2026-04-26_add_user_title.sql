-- Add title column for feature-level permissions.
-- Existing 'role' column continues to mean voter scope (ns/sb/d17/paw/admin).
-- 'title' is for feature access (admin/candidate/chief_of_staff/volunteer).

ALTER TABLE users ADD COLUMN title TEXT DEFAULT 'volunteer';

UPDATE users SET title = 'candidate' WHERE username = 'kcarriere';
UPDATE users SET title = 'volunteer' WHERE username = 'rcaron';
UPDATE users SET title = 'candidate' WHERE username = 'pjones';
UPDATE users SET title = 'volunteer' WHERE username = 'bjones';
UPDATE users SET title = 'candidate' WHERE username = 'rhammond';
UPDATE users SET title = 'chiefofstaff' WHERE username = 'ehammond';
UPDATE users SET title = 'chiefofstaff' WHERE username = 'slafontaine';
UPDATE users SET title = 'volunteer' WHERE username = 'alafontaine';
UPDATE users SET title = 'admin' WHERE username = 'nsdem';
UPDATE users SET title = 'candidate' WHERE username = 'nburdette';
UPDATE users SET title = 'chiefofstaff' WHERE username = 'wburdette';
UPDATE users SET title = 'candidate' WHERE username = 'tmcfeeters';
UPDATE users SET title = 'chiefofstaff' WHERE username = 'jmcfeeters';
UPDATE users SET title = 'candidate' WHERE username = 'gstelle';
UPDATE users SET title = 'candidate' WHERE username = 'mfasano';
UPDATE users SET title = 'chiefofstaff' WHERE username = 'afasano';
UPDATE users SET title = 'volunteer' WHERE username = 'test_ns';
UPDATE users SET title = 'volunteer' WHERE username = 'test_sb';
UPDATE users SET title = 'volunteer' WHERE username = 'test_d17';
UPDATE users SET title = 'volunteer' WHERE username = 'test_paw';
