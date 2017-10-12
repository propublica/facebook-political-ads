create or replace function to_englishtsquery(text text) returns tsquery as $$
begin
    return plainto_tsquery('english', text);
end;
$$ language plpgsql immutable;

create or replace function to_germantsquery(text text) returns tsquery as $$
begin
    return plainto_tsquery('german', text);
end;
$$ language plpgsql immutable;
