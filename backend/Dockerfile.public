FROM rust:latest

# this dockerfile appears to work and caches dependencies better

ENV SHELL /bin/bash

RUN apt-get update
RUN apt-get install libssl-dev -qqy
RUN apt-get install build-essential -qqy
RUN apt-get install pkgconf -qqy
RUN apt-get install libpq-dev -qqy
RUN apt-get install curl -qqy
RUN rm -rf /var/lib/apt/lists/*

RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN apt-get install -y nodejs



# create a new empty shell project, with our dependencies, to cache them.
RUN USER=root cargo new --bin server
WORKDIR /server
ADD server/Cargo.toml /server/Cargo.toml
ADD Cargo.lock /server/Cargo.lock
RUN cargo build --release
RUN rm src/*.rs

# okay now we're back in business
ADD server "/server"
# ADD tools "/tools"
# ADD Cargo.* "/"

RUN cargo build --release --all

ENV HOST "0.0.0.0:8080"
ENV RUST_LOG info
ENV ROOT "/server/"
ENV RUST_BACKTRACE 1
# ENV AWS_ACCESS_KEY_ID "AKIAwhatever"
# ENV AWS_SECRET_ACCESS_KEY "whatever"
ENV S3_BUCKET_NAME "pp-facebook-ads"
# S3_BUCKET_NAME is where to write images received from the plugin to. AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY must be ablle to write to it.

ENV DATABASE_URL "postgresql:///facebook_ads"
# change this to fit your URL

RUN cd public/ && npm install && npm run build

EXPOSE 80
CMD "./target/release/server"
