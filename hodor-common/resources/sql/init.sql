--------------------------------------------------------------------------------
-- init.sql: Initialize database
--------------------------------------------------------------------------------

-- Hodor database
BEGIN;

CREATE SCHEMA hodor;

--
-- Dor:
--
CREATE TABLE hodor.dor
  ( id                   SERIAL4              PRIMARY KEY UNIQUE
  , short_code           TEXT        NOT NULL
  , floor                INT2        NOT NULL
  , in_count             INT2        NOT NULL
  , out_count            INT2        NOT NULL
  , cumulative_in_count  INT2        NOT NULL
  , cumulative_out_count INT2        NOT NULL
  , timestamp            TIMESTAMPTZ NOT NULL
  , created_at           TIMESTAMPTZ          DEFAULT CURRENT_TIMESTAMP
  );

--
-- GNine:
--
CREATE TABLE hodor.gnine
( id                   SERIAL4              PRIMARY KEY UNIQUE
, short_code           TEXT        NOT NULL
, description          TEXT        NOT NULL
, address              TEXT        NOT NULL
, in_count             INT8        NOT NULL
, out_count            INT8        NOT NULL
, cumulative_in_count  INT8        NOT NULL
, cumulative_out_count INT8        NOT NULL
, timestamp            TIMESTAMPTZ NOT NULL
, created_at           TIMESTAMPTZ          DEFAULT CURRENT_TIMESTAMP
);

COMMIT;
