-- some setting to make the output less verbose
\set QUIET on
\set ON_ERROR_STOP on
set client_min_messages to warning;

-- load some variables from the env
\set anonymous `echo $DB_ANON_ROLE`
\set authenticator `echo $DB_USER`
\set authenticator_pass `echo $DB_PASS`


\echo # Loading database definition
begin;

\echo # Loading application definitions
-- private schema where all tables will be defined
-- you can use othere names besides "data" or even spread the tables
-- between different schemas. The schema name "data" is just a convention
\ir data/schema.sql
-- entities inside this schema (which should be only views and stored procedures) will be
-- exposed as API endpoints. Access to them however is still governed by the
-- privileges defined for the current PostgreSQL role making the requests
\ir api/schema.sql


\echo # Loading roles and privilege settings
\ir authorization/roles.sql
\ir authorization/privileges.sql

insert into data.trainer values (1, 'Ash', 'https://8bitocrafts.files.wordpress.com/2013/03/pokemon-red-sprite2.jpg');
insert into data.pokemon values (25, 'Pikachu', 'https://i.imgur.com/rPJT7Qa.png');
insert into data.capture values (1, 100, 25, 1);

commit;
\echo # ==========================================
