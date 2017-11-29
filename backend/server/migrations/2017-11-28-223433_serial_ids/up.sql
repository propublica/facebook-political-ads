create sequence targeting_infos_id_seq;
alter table targeting_infos alter column id set default nextval('targeting_infos_id_seq');
alter sequence targeting_infos_id_seq owned by targeting_infos.id;
