create table capture (
    id int primary key,
    level int check (1 <= level and level <= 100),
    pokemon_id int references pokemon(id) on delete cascade not null,
    trainer_id int references trainer(id) on delete cascade
);
