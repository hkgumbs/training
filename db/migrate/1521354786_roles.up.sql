-- Created at: 2018-03-18 01:33:06

BEGIN;
  CREATE ROLE anon;
  CREATE ROLE web;
  GRANT anon TO auth;
  GRANT web TO auth;
  GRANT usage ON SCHEMA "public" TO web;
COMMIT;
