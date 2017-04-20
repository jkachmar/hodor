--------------------------------------------------------------------------------
-- seed.sql: Seed hodor database values for test
--------------------------------------------------------------------------------

\c hodor hodor;

BEGIN;

INSERT INTO public.locations (short_code, team_id, store_id, description) VALUES
   ('NYC01',  1000, 2000, 'First Floor' )
 , ('NYC01',  1000, 2000, 'Second Floor')
 , ('LNDN01', 1000, 2000, 'First Floor - Lobby')
 , ('LAX01' , 1000, 2000, 'Roof Deck')
 ;

COMMIT;
