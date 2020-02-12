CREATE TABLE titles (
  tconst char(9) PRIMARY KEY,
  titleType text,
  primaryTitle text,
  originalTitle text,
  isAdult boolean,
  startYear integer,
  endYear integer,
  runtimeMinutes integer,
  genres text[]
);

CREATE TABLE names (
  nconst char(10) PRIMARY KEY,
  primaryName text,
  birthYear integer,
  deathYear integer,
  primaryProfession text[],
  knownForTitles char(9)[]
);

CREATE TABLE principals (
  tconst char(9),
  ordering integer,
  nconst char(10),
  category text,
  job text,
  characters text
);