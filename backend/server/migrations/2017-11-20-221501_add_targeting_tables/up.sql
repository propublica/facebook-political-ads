create table targeting_info (
  id integer primary key not null,
  targeting text not null,
  segment text
);
create unique index index_targeting_info_on_targeting_and_segment on targeting_info(targeting, segment);

create table ads_targeting (
  ad_id text references ads(id) on delete restrict,
  targeting_id integer references targeting_info(id) on delete cascade,
  primary key (ad_id, targeting_id)
);
