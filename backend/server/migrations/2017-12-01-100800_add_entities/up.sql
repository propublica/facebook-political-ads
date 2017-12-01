alter table ads add column entities jsonb;
create index index_ads_on_entities on ads using gin(entities);
