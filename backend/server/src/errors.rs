use r2d2::GetTimeout;
use diesel::result::Error as diesel_error;
use serde_json::Error as serde_error;
use string::FromUtf8Error;
use hyper::Error as hyper_error;
use hyper::error::UriError;
use rusoto_core::TlsError;
use rusoto_s3::PutObjectError;
use rusoto_credential::CredentialsError;


// This is basically a catch all for all the Errors we think we'll ever see to
// combat type restrictions on all of the hyper/futures function calls.
error_chain! {
    foreign_links {
        Timeout(GetTimeout);
        DataBase(diesel_error);
        JSON(serde_error);
        String(FromUtf8Error);
        Hyper(hyper_error);
        Uri(UriError);
        TLS(TlsError);
        S3(PutObjectError);
        AWS(CredentialsError);
    }

    errors {
        HTML(m: String) {
            description("Invalid HTML"),
            display("Invalid HTML: {}", m),
        }
    }
}
