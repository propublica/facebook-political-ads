alter table ads add column suppressed boolean not null default false;
create index index_ads_on_suppressed on ads(suppressed);
