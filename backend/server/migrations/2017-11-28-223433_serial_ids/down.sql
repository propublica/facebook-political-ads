alter table targeting_infos alter column id type integer;
alter table targeting_infos alter column id drop default;
drop sequence targeting_infos_id_seq;
