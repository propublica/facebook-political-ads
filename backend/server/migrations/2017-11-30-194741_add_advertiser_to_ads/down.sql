drop index index_ads_on_pages;
alter table ads drop column pages;
drop index index_ads_on_advertiser;
alter table ads drop column advertiser;
