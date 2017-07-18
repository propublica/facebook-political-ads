alter table ads add column images text array;
alter table ads drop column big_image;
alter table ads rename image to thumbnail;
