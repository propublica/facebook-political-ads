alter table ads add column fuzzy_id integer;
create index index_ads_on_fuzzy_id on ads(fuzzy_id);
