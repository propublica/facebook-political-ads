alter table targeting_infos rename column targeting_type to targeting;
alter table targeting_infos rename to targeting_info;
alter table ads_targeting_infos rename column targeting_info_id to targeting_id;
alter table ads_targeting_infos rename to ads_targeting;
