-- We're doing this so that we can use diesel to build out our tsvectors, and utilize psql's indices
create or replace function to_englishtsvector(text text) returns tsvector as $$
begin
    return to_tsvector('english', text);
end;
$$ language plpgsql immutable;

create or replace function to_germantsvector(text text) returns tsvector as $$
begin
    return to_tsvector('german', text);
end;
$$ language plpgsql immutable;
