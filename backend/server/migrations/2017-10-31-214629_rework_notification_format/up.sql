create or replace function notify_update() returns trigger as $$
declare
field text := 'update';
begin
    if (NEW.political_probability > 0.80) then
        case
          when OLD.political_probability != NEW.political_probability then
            field := 'political_probability';
          when OLD.impressions != NEW.impressions then
            field := 'impressions';
          when OLD.political != NEW.political then
            field := 'political';
          when OLD.not_political != NEW.not_political then
            field := 'not_political';
          when OLD.suppressed != NEW.suppressed then
            field := 'delete';
        end case;
        PERFORM pg_notify('ad_update',
                          'event: ' || field || E'\n' ||
                          'data: ' || json_build_object('id', NEW.id)::text || E'\n\n');
    end if;

    return NEW;
end;
$$ LANGUAGE plpgsql;
