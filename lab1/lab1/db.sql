DROP TABLE IF EXISTS cabinets;
DROP TABLE IF EXISTS students;
DROP TABLE IF EXISTS teachers;
DROP TABLE IF EXISTS workplaces;
DROP TABLE IF EXISTS class;

CREATE TABLE IF NOT EXISTS cabinets
(
    num INT PRIMARY KEY,
    begin_ TIMESTAMP,
    end_ TIMESTAMP
);

CREATE TABLE IF NOT EXISTS students
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(20)
);

CREATE TABLE IF NOT EXISTS teachers
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(20)
);


CREATE TABLE IF NOT EXISTS workplaces
(
    id SERIAL PRIMARY KEY,
    cabinet_num INT REFERENCES cabinets (num),
    student_id INT
);

CREATE TABLE IF NOT EXISTS class
(
    id SERIAL PRIMARY KEY,
    cabinet_num INT REFERENCES cabinets (num),
    teacher_id INT REFERENCES teachers (id),
    begin_ TIMESTAMP,
    end_ TIMESTAMP
);

INSERT INTO teachers
	(name)
values ('Rob'), ('Bob'), ('Mark');

INSERT INTO students
	(name)
values ('s_1'), ('s_2'), ('s_3');