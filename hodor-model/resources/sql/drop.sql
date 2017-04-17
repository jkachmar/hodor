--------------------------------------------------------------------------------
-- drop.sql: Drop database
--------------------------------------------------------------------------------

BEGIN;

DROP TABLE hodor.dor;
DROP TABLE hodor.gnine;
DROP SCHEMA hodor;

COMMIT;
