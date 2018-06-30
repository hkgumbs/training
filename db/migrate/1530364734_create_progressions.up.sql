BEGIN;
  ALTER TABLE public.movements DROP COLUMN exercise_id;

  CREATE TABLE public.progressions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v1mc(),
    from_movement_id UUID REFERENCES movements,
    to_movement_id UUID REFERENCES movements,
    exercise_id UUID REFERENCES exercises,
    rate INTEGER NOT NULL
  );
  GRANT SELECT ON public.progressions TO web;
COMMIT;