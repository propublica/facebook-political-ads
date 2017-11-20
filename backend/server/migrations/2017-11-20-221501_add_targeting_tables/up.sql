create type targeting_type as enum (
  'Gender',
  'City',
  'State',
  'Region'
  'Age',
  'Interest',
  'Segment',
  'Advertiser',
  'Retargeting',
  'Agency',
  'Website',
  'Language',
  'Employer',
  'School',
  'Like',
  'List',
);

create table targeting (
  id integer primary key not null,
  targeting targeting_type,
  segment string
);
create unique index index_targeting_on_targeting_and_segment(targeting, segment);

create table ads_targeting (
  ad_id text references ads(id),
  targeting_id integer references targeting(id),
  primary key (ad_id, targeting_id)
);
