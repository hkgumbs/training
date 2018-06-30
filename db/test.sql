INSERT INTO exercises (name) VALUES ('Deadlift');

INSERT INTO exercise_features (
  exercise_id, value
) VALUES
  (..., 'hip-dominent'),
  (..., 'saggital'),
  (..., 'multi-joint')
;

INSERT INTO public.movements (
    name, sets, reps, rest, load
) VALUES
  ('1/2 Foam Roller Hamstring Stretch', 1, 15, 0, NULL),
  ('Active Straight Leg Raise With Assist ', 1, 12, 0, NULL),
  ('Active Straight Leg Raise Without Assist ', 1, 12, 0, NULL),
  ('Glute Bridges on Back', 1, 15, 0, NULL),
  ('Foam Roll Hip Hinge', 2, 20, 30, NULL)
;

-- TODO: create progressions