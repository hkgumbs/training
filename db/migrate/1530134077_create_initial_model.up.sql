BEGIN;
  CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

  CREATE TYPE load_unit_type AS ENUM ('LB', 'KG');
  CREATE TYPE load_type AS (amount INTEGER, unit load_unit_type);

  --

  CREATE TABLE public.exercises (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v1mc(),
    name TEXT NOT NULL
  );

  CREATE TABLE public.movements (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v1mc(),
    exercise_id UUID REFERENCES exercises,
    name TEXT NOT NULL,
    sets INTEGER NOT NULL,
    reps INTEGER NOT NULL,
    rest INTEGER NOT NULL,
    load load_type
  );

  CREATE TABLE public.exercise_features (
    exercise_id UUID REFERENCES exercises,
    value text
  );

  --

  GRANT SELECT ON public.exercises TO web;
  GRANT SELECT ON public.movements TO web;
  GRANT SELECT ON public.exercise_features TO web;
COMMIT;
