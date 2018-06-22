-- Created at: 2018-03-18 01:33:06

BEGIN;
  REVOKE USAGE ON SCHEMA "public" FROM web;
  DROP ROLE anon;
  DROP ROLE web;
COMMIT;
