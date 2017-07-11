use super::schema::ads;

#[derive(Queryable)]
pub struct Ad {
    pub id: String,
    pub html: String,
    pub political: i32,
    pub not_political: i32,

    pub fuzzy_id: Option<i32>,
    pub title: Option<String>,
    pub message: Option<String>,
    pub image: Option<String>,
    pub big_image: Option<String>,
}

#[derive(Insertable)]
#[table_name = "ads"]
pub struct NewAd<'a> {
    pub id: &'a str,
    pub html: &'a str,
    pub political: i32,
    pub not_political: i32,
}
