drop schema if exists data cascade;
create schema data;
set search_path = data, public;

\ir pokemon.sql
\ir trainer.sql
\ir capture.sql
