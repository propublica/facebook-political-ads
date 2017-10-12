drop index index_ads_on_english_html;
drop index index_ads_on_german_html;
create index index_ads_on_html on ads using gin(to_tsvector('english', html));
