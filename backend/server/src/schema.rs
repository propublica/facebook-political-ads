// $ diesel print-schema, then copy-paste it here.
table! {
    ads (id) {
        id -> Text,
        html -> Text,
        political -> Int4,
        not_political -> Int4,
        title -> Text,
        message -> Text,
        thumbnail -> Text,
        created_at -> Timestamptz,
        updated_at -> Timestamptz,
        lang -> Text,
        images -> Array<Text>,
        impressions -> Int4,
        political_probability -> Float8,
        targeting -> Nullable<Text>,
        suppressed -> Bool,
        targets -> Nullable<Jsonb>,
        advertiser -> Nullable<Text>,
        entities -> Nullable<Jsonb>,
        page -> Nullable<Text>,
        lower_page -> Nullable<Text>,
        paid_for_by -> Nullable<Text>,
        targetings -> Nullable<Array<Text>>,
        targetedness -> Nullable<Int4>,
    }
}