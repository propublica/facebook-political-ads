alter table ads add column created_at timestamptz not null default now();
alter table ads add column updated_at timestamptz not null default now();
