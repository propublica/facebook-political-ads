alter table ads add column pages jsonb;
create index index_ads_on_pages on ads using gin(pages);
alter table ads add column advertiser text;
create index index_ads_on_advertiser on ads(advertiser);
