drop index index_ads_on_pages;
alter table ads drop column pages;
alter table ads add column page text;
create index index_ads_on_page on ads(page);
