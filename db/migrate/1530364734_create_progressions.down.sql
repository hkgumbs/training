BEGIN;
  ALTER TABLE public.movements ADD COLUMN exercise_id UUID REFERENCES exercises,
  DROP TABLE public.progressions;
COMMIT;