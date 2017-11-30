create table targeting_infos (
  id integer primary key not null,
  targeting text not null,
  segment text
);
create unique index index_targeting_infos_on_targeting_and_segment on targeting_infos(targeting, segment);

create table ads_targeting_infos (
  ad_id text references ads(id) on delete restrict,
  targeting_info_id integer references targeting_infos(id) on delete cascade,
  primary key (ad_id, targeting_info_id)
);
