infer_schema!("dotenv:DATABASE_URL");

// Dummy tables for derived queries
table! {
    use diesel::types::{BigInt, Text};
    aggregate_targetings (count) {
        count -> BigInt,
        targeting_type -> Text,
    }
}

#[derive(QueryableByName, Debug, Clone)]
pub struct AggregateTargeting {
    pub count: i64,
    pub targeting_type: String,
}
