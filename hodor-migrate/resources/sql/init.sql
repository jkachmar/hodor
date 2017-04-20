--------------------------------------------------------------------------------
-- init.sql: Initialize hodor's database
--------------------------------------------------------------------------------

CREATE DATABASE hodor;
CREATE USER     hodor;

GRANT ALL PRIVILEGES ON DATABASE hodor TO hodor;
