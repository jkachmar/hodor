module Main where

import           ClassyPrelude
import           Database.PostgreSQL.Simple ()
import           Refurb                     (ConnInfo (ConnInfo), Migration,
                                             MonadMigration, execute_, qqSql,
                                             refurbMain, schemaMigration)

main :: IO ()
main =
  refurbMain
    (const . pure $ ConnInfo "localhost" 5432 "hodor" "" "hodor")
    migrations

migrations :: [Migration]
migrations =
  [ schemaMigration "public" "create-location-table" createLocationTable
  , schemaMigration "public" "create-dor-table"      createDorTable
  , schemaMigration "public" "associate-foreign-key" assocForeignKey
  ]

createLocationTable :: MonadMigration m => m ()
createLocationTable =
  let createLocTblQ =
        [qqSql|
          CREATE TABLE public.locations
            ( id          SERIAL4              PRIMARY KEY UNIQUE
            , team_id     INT4        NOT NULL
            , store_id    INT4        NOT NULL
            , short_code  TEXT        NOT NULL
            , description TEXT        NOT NULL
            , created_at  TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
            );
        |]
  in void $ execute_ createLocTblQ

createDorTable :: MonadMigration m => m ()
createDorTable =
  let createDorTblQ =
        [qqSql|
          CREATE TABLE public.dor
            ( id                   SERIAL4              PRIMARY KEY UNIQUE
            , location_id          INT4        NOT NULL
            , store_id             INT4        NOT NULL
            , in_count             INT2        NOT NULL
            , out_count            INT2        NOT NULL
            , cumulative_in_count  INT2        NOT NULL
            , cumulative_out_count INT2        NOT NULL
            , timestamp            TIMESTAMPTZ NOT NULL
            , created_at           TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
            );
        |]
  in void $ execute_ createDorTblQ

assocForeignKey :: MonadMigration m => m ()
assocForeignKey =
  let assocFKeyQ =
        [qqSql|
          ALTER TABLE    public.dor
          ADD CONSTRAINT dor_location_id_fkey
          FOREIGN KEY    (location_id)
          REFERENCES     locations(id);
        |]
  in void $ execute_ assocFKeyQ
