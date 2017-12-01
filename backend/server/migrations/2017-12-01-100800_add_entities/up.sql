alter table ads add column entities jsonb;
create index index_ads_on_entites on ads using gin(entities);