BEGIN;
  DROP EXTENSION IF EXISTS "uuid-ossp";

  DROP TYPE dominance;
  DROP TYPE joint_action;
  DROP TYPE load_unit;
  DROP TYPE load;
  DROP TYPE movement_option;

  DROP TABLE exercises;
  DROP TABLE movements;
  DROP TABLE exercise_features;
COMMIT;
