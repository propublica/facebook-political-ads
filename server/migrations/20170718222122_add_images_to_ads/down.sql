alter table ads drop column images;
alter table ads add column big_image text;
alter table ads rename thumbnail to image;
