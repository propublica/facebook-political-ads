alter table ads alter column entities set default '[]'::jsonb;
alter table ads alter column targets set default '[]'::jsonb;
alter table ads alter column pages drop default;
alter table ads alter column pages type text array using [];
alter table ads alter column pages set default text[];
update ads set entities = '[]'::jsonb;
update ads set targets = '[]'::jsonb;
