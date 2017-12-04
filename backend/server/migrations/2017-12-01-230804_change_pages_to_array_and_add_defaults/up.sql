alter table ads alter column entities set default '[]'::jsonb;
alter table ads alter column targets set default '[]'::jsonb;
alter table ads alter column pages drop default;
alter table ads alter column pages type text array using array[]::text[];
alter table ads alter column pages set default '{}';
update ads set entities = '[]'::jsonb;
update ads set targets = '[]'::jsonb;
