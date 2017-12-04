drop index index_ads_on_page;
alter table ads drop column page;
alter table ads add column pages type text array using array[]::text[];
alter table ads alter column pages set default '{}';
add index index_ads_on_pages;
