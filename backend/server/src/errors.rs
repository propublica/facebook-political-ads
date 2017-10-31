use r2d2::GetTimeout;
use diesel::result::Error as DieselError;
use serde_json::Error as SerdeError;
use string::FromUtf8Error;
use hyper::Error as HyperError;
use hyper::error::UriError;
use tokio_postgres::Error as TokioPostgresError;
use rusoto_core::TlsError;
use rusoto_s3::PutObjectError;
use rusoto_credential::CredentialsError;


// This is basically a catch all for all the Errors we think we'll ever see to
// combat type restrictions on all of the hyper/futures function calls.
error_chain! {
    foreign_links {
        Timeout(GetTimeout);
        DataBase(DieselError);
        JSON(SerdeError);
        String(FromUtf8Error);
        Hyper(HyperError);
        Uri(UriError);
        TLS(TlsError);
        S3(PutObjectError);
        AWS(CredentialsError);
        Notifications(TokioPostgresError);
    }

    errors {
        HTML(m: String) {
            description("Invalid HTML"),
            display("Invalid HTML: {}", m),
        }
    }
}
