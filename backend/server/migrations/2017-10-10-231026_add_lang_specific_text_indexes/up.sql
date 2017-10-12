drop index index_ads_on_html;
create index index_ads_on_english_html on ads using gin(to_englishtsvector(html));
create index index_ads_on_german_html on ads using gin(to_germantsvector(html));
