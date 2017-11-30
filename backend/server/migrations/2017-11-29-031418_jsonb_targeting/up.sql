alter table ads add column targets jsonb;
create index index_ads_on_targets on ads using gin(targets);
