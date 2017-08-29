extern crate murmurhash3;
extern crate regex;
extern crate rusty_machine;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use murmurhash3::murmurhash3_x86_32;
use regex::Regex;
use rusty_machine::linalg::Vector;
use rusty_machine::linalg::Metric;

static TOKEN_PATTERN: &'static str = r"\b\w\w+\b";

#[derive(Deserialize)]
struct ClassifierFromJSON {
    // Not sure why the JSON dump from sklearn has this as a vec of a vector
    feature_log_prob: Vec<Vec<f64>>,
    class_log_prior: [f64; 2],
}

pub struct Classifier {
    feature_log_prob: Vector<f64>,
    class_log_prior: [f64; 2],
}


impl Classifier {
    /// Returns the index for a given feature.
    ///
    /// Based on sklearn's HashingVectorizer, which uses
    /// abs(murmurhash3(token))%n_features to determine
    /// index of feature.
    pub fn get_feature_idx(token: &str, n_features: i32) -> usize {
        let hash = murmurhash3_x86_32(token.as_bytes(), 0);
        ((hash as i32).abs() % n_features) as usize
    }

    pub fn tokenize_string(string: &str) -> Vec<&str> {
        Regex::new(TOKEN_PATTERN)
            .unwrap()
            .find_iter(string)
            .map(|x| x.as_str())
            .collect::<Vec<&str>>()
    }

    pub fn create_feature_vec(doc: &str, n_features: i32) -> Vector<f64> {
        let mut feature_vec = vec![0.0; (n_features as usize)];
        let lowercase_doc = &doc.to_lowercase();
        let tokens = Classifier::tokenize_string(&lowercase_doc);

        for tok in tokens {
            feature_vec[Classifier::get_feature_idx(tok, n_features)] += 1.0;
        }

        // L2 norm
        let euc_norm = Vector::new(feature_vec.to_vec()).norm();
        Vector::new(feature_vec.to_vec().iter().map(|x| x / euc_norm).collect())
    }

    // Returns the log probability of a document
    pub fn classify(&self, doc: &str) -> [f64; 2] {
        let features = Classifier::create_feature_vec(doc, 10000);
        let dot = features.dot(&self.feature_log_prob);
        [
            (self.class_log_prior[0] + dot).exp(),
            (self.class_log_prior[1] + dot).exp(),
        ]
    }

    pub fn from_json(json: String) -> Classifier {
        let read: ClassifierFromJSON = serde_json::from_str(&json).unwrap();
        Classifier {
            feature_log_prob: Vector::new(read.feature_log_prob[0].clone()),
            class_log_prior: read.class_log_prior,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Classifier;

    #[test]
    fn test_get_feature_idx() {
        let idx = Classifier::get_feature_idx("Harold", 10);
        assert_eq!(idx, 4);

        let idx = Classifier::get_feature_idx("Clinton", 10);
        assert_eq!(idx, 3);

        let idx = Classifier::get_feature_idx("foobar", 10);
        assert_eq!(idx, 5);

        let idx = Classifier::get_feature_idx("vote", 10);
        assert_eq!(idx, 6);

    }

    #[test]
    fn test_tokenize_string() {
        let test_string = "The quick brown fox jumped over the lazy dog";


        assert_eq!(
            vec![
                "The",
                "quick",
                "brown",
                "fox",
                "jumped",
                "over",
                "the",
                "lazy",
                "dog",
            ],
            Classifier::tokenize_string(test_string)
        );
    }

    #[test]
    fn test_create_feature_vec() {
        let test_string = "The quick brown fox jumped over the lazy dog";
        let res = Classifier::create_feature_vec(test_string, 10);

        assert_eq!(
            vec![
                0.0,
                0.22941573,
                0.0,
                0.45883146,
                0.0,
                0.45883146,
                0.0,
                0.22941573,
                0.6882472,
                0.0,
            ],
            res
        )
    }
}
