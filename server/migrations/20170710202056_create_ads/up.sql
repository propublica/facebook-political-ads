create table ads (
  id text primary key not null,
  html text not null,
  political integer not null,
  not_political integer not null,

  fuzzy_id integer,
  title text,
  message text,
  image text,
  big_image text
);
create index index_ads_on_fuzzy_id on ads(fuzzy_id);
