drop index index_targeting_info_on_targeting_and_segment;
create unique index index_targeting_infos_on_targeting_and_segment on targeting_infos(targeting_type, coalesce(segment, 'just a dummy string to circumvent this: https://stackoverflow.com/questions/8289100/create-unique-constraint-with-null-columns'));
