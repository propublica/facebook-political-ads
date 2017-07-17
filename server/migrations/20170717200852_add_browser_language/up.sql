alter table ads add column browser_lang text not null;
create index index_ads_on_browser_lang on ads(browser_lang);
