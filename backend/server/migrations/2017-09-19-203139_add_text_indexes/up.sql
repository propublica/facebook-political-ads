create index index_ads_on_html on ads using gin(to_tsvector('english', html));
