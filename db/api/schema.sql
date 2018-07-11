drop schema if exists api cascade;
create schema api;
set search_path = api, public;

\ir schools.sql
