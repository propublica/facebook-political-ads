use diesel::r2d2::PoolError as R2Error;
use diesel::result::Error as DieselError;
use serde_json::Error as SerdeError;
use string::FromUtf8Error;
use hyper::Error as HyperError;
use hyper::error::UriError;
use tokio_postgres::Error as TokioPostgresError;
use rusoto_core::TlsError;
use rusoto_s3::PutObjectError;
use rusoto_credential::CredentialsError;
use nom::simple_errors::Err;
use nom::Needed;

// This is basically a catch all for all the Errors we think we'll ever see to
// combat type restrictions on all of the hyper/futures function calls.
error_chain! {
    foreign_links {
        PoolError(R2Error);
        DataBase(DieselError);
        JSON(SerdeError);
        String(FromUtf8Error);
        Hyper(HyperError);
        Uri(UriError);
        TLS(TlsError);
        S3(PutObjectError);
        AWS(CredentialsError);
        Notifications(TokioPostgresError);
        Targeting(Err);
    }

    errors {
        HTML(m: String) {
            description("Invalid HTML"),
            display("Invalid HTML: {}", m),
        }

        TargetingIncomplete(e: Needed) {
            description("Not enough data for Targeting Parser"),
            display("Insufficient Data: {:?}", e),
        }
    }
}
