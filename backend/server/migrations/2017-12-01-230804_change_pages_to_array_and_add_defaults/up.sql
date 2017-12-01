alter table ads alter column entities set default '[]'::jsonb;
alter table ads alter column targeting set default '[]'::jsonb;
alter table ads alter column pages drop default;
alter table ads alter column pages type text array using [];
alter table ads alter column pages set default text[];
