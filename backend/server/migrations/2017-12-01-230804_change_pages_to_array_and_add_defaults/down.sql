alter table ads alter column entities drop default;
alter table ads alter column targeting drop default;
-- these are irreversible so we'll just error
-- alter table ads alter column pages drop default;
alter table ads alter column pages type jsonb;
