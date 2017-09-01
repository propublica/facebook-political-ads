#[macro_use]
extern crate error_chain;
extern crate murmurhash3;
extern crate regex;
extern crate rusty_machine;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use murmurhash3::murmurhash3_x86_32;
use regex::Regex;
use rusty_machine::prelude::BaseMatrix;
use rusty_machine::linalg::Vector;
use rusty_machine::linalg::Matrix;
use rusty_machine::linalg::Metric;
use std::fs::File;

static TOKEN_PATTERN: &'static str = r"\b\w\w+\b";

#[derive(Deserialize)]
pub struct JSONClassifier {
    feature_log_prob: Vec<f64>,
    class_log_prior: Vec<f64>,
    n_features: i32,
    n_classes: i32,
}

pub struct Classifier {
    n_features: i32,
    feature_log_prob: Matrix<f64>,
    class_log_prior: Matrix<f64>,
}

error_chain! {
    foreign_links {
        JSON(serde_json::Error);
        Io(::std::io::Error);
    }
}

impl Classifier {
    // Create matrices for classifier
    pub fn new(
        feature_log_prob: Vec<f64>,
        class_log_prior: Vec<f64>,
        n_features: i32,
        n_classes: i32,
    ) -> Classifier {
        let feature_log_mat =
            Matrix::new(n_classes as usize, n_features as usize, feature_log_prob);

        let class_prior_mat = Matrix::new(1, n_classes as usize, class_log_prior);

        Classifier {
            n_features: n_features,
            feature_log_prob: feature_log_mat,
            class_log_prior: class_prior_mat,
        }
    }

    fn joint_log_likelihood(&self, doc: &str) -> Matrix<f64> {
        let feature_vec = Matrix::new(
            1,
            self.n_features as usize,
            create_feature_vec(doc, self.n_features),
        );
        let res = (feature_vec * self.feature_log_prob.transpose()) + &(self.class_log_prior);
        res
    }

    pub fn predict(&self, doc: &str) -> usize {
        let jll = self.joint_log_likelihood(doc);
        let mut class = 0;
        let mut cur_max = -1.0f64 / 0.0f64; // NEG_INFINITY
        for (i, &x) in jll.into_vec().iter().enumerate() {
            if x > cur_max {
                class = i;
                cur_max = x;
            }
        }
        class
    }

    pub fn predict_likelihoods(&self, doc: &str) -> Vec<f64> {
        let jll = self.joint_log_likelihood(doc);
        let mut acc = 0.0;
        for &x in jll.clone().into_vec().iter() {
            acc += x.exp();
        }
        acc = acc.ln();
        let likelihoods = jll.into_vec().iter().map(|x| (x - acc).exp()).collect();
        likelihoods
    }

    pub fn from_json(file: &str) -> Result<Classifier> {
        let file = File::open(file)?;
        let read: JSONClassifier = serde_json::from_reader(file)?;
        Ok(Classifier::new(
            read.feature_log_prob,
            read.class_log_prior,
            read.n_features,
            read.n_classes,
        ))
    }
}

/// Returns the index for a given feature.
///
/// Based on sklearn's HashingVectorizer, which uses
/// abs(murmurhash3(token))%n_features to determine
/// index of feature.
pub fn get_feature_idx(token: &str, n_features: i32) -> usize {
    let hash = murmurhash3_x86_32(token.as_bytes(), 0);
    return ((hash as i32).abs() % n_features) as usize;
}

pub fn tokenize_string(string: &str) -> Vec<&str> {
    let tokens: Vec<&str> = Regex::new(TOKEN_PATTERN)
        .unwrap()
        .find_iter(string)
        .map(|x| x.as_str())
        .collect();
    return tokens;
}

pub fn create_feature_vec(doc: &str, n_features: i32) -> Vec<f64> {
    let mut feature_vec = vec![0.0; (n_features as usize)];
    let lowercase_doc = &doc.to_lowercase();
    let tokens = tokenize_string(&lowercase_doc);

    for tok in tokens {
        feature_vec[get_feature_idx(tok, n_features)] += 1.0;
    }

    // L2 norm
    let mut normalized_vec = feature_vec.to_vec();
    let euc_norm = Vector::new(feature_vec).norm();
    normalized_vec = normalized_vec.iter().map(|x| x / euc_norm).collect();

    return normalized_vec;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_feature_idx() {
        let idx = get_feature_idx("Harold", 10);
        assert_eq!(idx, 4);

        let idx = get_feature_idx("Clinton", 10);
        assert_eq!(idx, 3);

        let idx = get_feature_idx("foobar", 10);
        assert_eq!(idx, 5);

        let idx = get_feature_idx("vote", 10);
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
            tokenize_string(test_string)
        );
    }

    #[test]
    fn test_create_feature_vec() {
        let test_string = "The quick brown fox jumped over the lazy dog";
        let res = create_feature_vec(test_string, 10);

        assert_eq!(
            vec![
                0.0,
                0.22941573387056174,
                0.0,
                0.4588314677411235,
                0.0,
                0.4588314677411235,
                0.0,
                0.22941573387056174,
                0.6882472016116852,
                0.0,
            ],
            res
        )
    }

    #[test]
    fn test_prediction() {
        let test_pol_string = "Die Wähler sollten bei der Wahl abstimmen.";
        let test_non_pol_string = "This is nothing important, just watch netflix";
        let clf = Classifier::from_json("data/de/classifier.json").unwrap();
        assert_eq!(1, clf.predict(test_pol_string));
        assert_eq!(0, clf.predict(test_non_pol_string));
    }

    #[test]
    fn test_predict_likelihoods() {
        let clf = Classifier::from_json("data/de/classifier.json").unwrap();
        let test_pol_string_1 = "Die Wähler sollten bei der Wahl abstimmen.";
        let test_pol_string_2 = "Die Wahl ist im September. Denken Sie daran, Ihren Kandidaten zu wählen.";
        let test_non_pol_string_1 = "This is nothing important, just watch netflix";
        let test_non_pol_string_2 = "Kaufen Sie Ihr Lieblingsessen. Das ist eine langweilige Werbung.";
        let likelihoods = clf.predict_likelihoods(test_pol_string_1);
        assert!(likelihoods[1] > likelihoods[0]);
        let likelihoods = clf.predict_likelihoods(test_pol_string_2);
        assert!(likelihoods[1] > likelihoods[0]);
        let likelihoods = clf.predict_likelihoods(test_non_pol_string_1);
        assert!(likelihoods[0] > likelihoods[1]);
        let likelihoods = clf.predict_likelihoods(test_non_pol_string_2);
        assert!(likelihoods[0] > likelihoods[1]);
        assert!(((likelihoods[0] + likelihoods[1]) - 1.0) < 0.0000001);
    }
}
