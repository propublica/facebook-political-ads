alter table ads
      alter column title set not null,
      alter column message set not null,
      alter column thumbnail set not null,
      alter column images set not null;
