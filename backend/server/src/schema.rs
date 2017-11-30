infer_schema!("dotenv:DATABASE_URL");

// Dummy tables for derived queries
table! {
    use diesel::types::{Integer, Text};
    aggregate_targetings (count) {
        count -> Integer,
        targeting_type -> Text,
    }
}

#[derive(QueryableByName, Debug, Clone)]
pub struct AggregateTargeting {
    pub count: i32,
    pub targeting_type: String,
}
