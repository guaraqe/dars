CREATE TYPE user_role AS ENUM ('Teacher', 'Student', 'Administration');

CREATE TABLE users
  ( id uuid PRIMARY KEY DEFAULT gen_random_uuid()
  , role user_role NOT NULL
  , first_name text NOT NULL
  , middle_name text NOT NULL
  , family_name text NOT NULL
  , email text NOT NULL
  , phone text NOT NULL
  );

CREATE TABLE periods
  ( id uuid PRIMARY KEY DEFAULT gen_random_uuid()
  , name text NOT NULL
  , start_date date NOT NULL
  , end_date date NOT NULL
  );

CREATE TABLE courses
  ( id uuid PRIMARY KEY DEFAULT gen_random_uuid()
  , name text NOT NULL
  , description text NOT NULL
  );

CREATE TABLE course_parts
  ( id uuid PRIMARY KEY DEFAULT gen_random_uuid()
  , course_id uuid NOT NULL REFERENCES courses(id)
  , name text NOT NULL
  , description text NOT NULL
  , graded bool NOT NULL
  , weight integer
  );

CREATE TABLE course_units
  ( id uuid PRIMARY KEY DEFAULT gen_random_uuid()
  , course_part_id uuid NOT NULL REFERENCES course_parts(id)
  , unit_date date NOT NULL
  , unit_start_time time
  , unit_end_time time
  , unit_duration interval
  , name text NOT NULL
  , description text NOT NULL
  , graded bool NOT NULL
  );

CREATE TABLE course_teachers
  ( id uuid PRIMARY KEY DEFAULT gen_random_uuid()
  , course_id uuid NOT NULL REFERENCES courses(id)
  , teacher_id uuid NOT NULL REFERENCES users(id)
  );

CREATE TABLE course_students
  ( id uuid PRIMARY KEY DEFAULT gen_random_uuid()
  , course_id uuid NOT NULL REFERENCES courses(id)
  , student_id uuid NOT NULL REFERENCES users(id)
  );

CREATE TABLE course_units_students
  ( id uuid PRIMARY KEY DEFAULT gen_random_uuid()
  , course_unit_id uuid NOT NULL REFERENCES course_units(id)
  , student_id uuid NOT NULL REFERENCES users(id)
  , present boolean NOT NULL
  , grade int
  );
