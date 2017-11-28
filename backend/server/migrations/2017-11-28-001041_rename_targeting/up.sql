alter table targeting_info rename column targeting to targeting_type;
alter table targeting_info rename to targeting_infos;
alter table ads_targeting rename column targeting_id to targeting_info_id;
alter table ads_targeting rename to ads_targeting_infos;
