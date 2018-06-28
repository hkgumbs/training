BEGIN;
  CREATE ROLE anon nologin;
  CREATE ROLE web nologin;
  GRANT anon TO auth;
  GRANT web TO auth;
  GRANT usage ON SCHEMA "public" TO web;
COMMIT;
