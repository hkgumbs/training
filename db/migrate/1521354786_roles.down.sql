BEGIN;
  REVOKE USAGE ON SCHEMA "public" FROM web;
  DROP ROLE anon;
  DROP ROLE web;
COMMIT;
