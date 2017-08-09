alter table ads
      alter column title drop not null,
      alter column message drop not null,
      alter column thumbnail drop not null,
      alter column images drop not null;
