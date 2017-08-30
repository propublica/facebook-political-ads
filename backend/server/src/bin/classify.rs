extern crate classifier;
use classifier::Classifier;
use diesel::pg::PgConnection;
use dotenv::dotenv;
use self::models::Ad;

// For now we grab all the ads in the database and classify and resave them not
// ideal in the future, but we're launching next week.
fn main() {
    dotenv().ok();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).expect("could not connect to the databas");
    let ads: Vec<Ad> = diesel::select
}
