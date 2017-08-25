extern crate murmurhash3;
extern crate regex;
extern crate rusty_machine;

use murmurhash3::murmurhash3_x86_32;
use regex::Regex;
use rusty_machine::linalg::Vector;
use rusty_machine::linalg::Metric;

static TOKEN_PATTERN: &'static str =r"\b\w\w+\b";

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
    let tokens : Vec<&str> = Regex::new(TOKEN_PATTERN).unwrap().find_iter(string)
                                    .map(|x| x.as_str()).collect();
    return tokens;                                      
}

pub fn create_feature_vec(doc: &str, n_features: i32) -> Vec<f32> {
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
    use super::get_feature_idx;
    use super::create_feature_vec;
    use super::tokenize_string;

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
        

        assert_eq!(vec!["The", "quick", "brown", 
                        "fox", "jumped", "over", 
                        "the", "lazy", "dog"],
                    tokenize_string(test_string));
    }

    #[test]
    fn test_create_feature_vec() {
        let test_string = "The quick brown fox jumped over the lazy dog";
        let res = create_feature_vec(test_string, 10);

        assert_eq!(vec![0.0, 0.22941573, 0.0, 
                    0.45883146, 0.0, 0.45883146,
                    0.0, 0.22941573, 0.6882472, 0.0],
                    res)
    }
}
