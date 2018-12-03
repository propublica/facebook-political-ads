Architecture
============

The Facebook Political Ad Collector has several pieces. This document describes the various services required to run a version of FBPAC on your own, and how those services map to bits of code.

[INSTALLATION.md](INSTALLATION.md) discusses how to actually install this (locally or in production); this document provides the overview of what you'll eventually have running.

Repos
-----
- [https://github.com/propublica/facebook-political-ads](https://github.com/propublica/facebook-political-ads)
	- `backend/server` repo Rust app. Receives ads and ad ratings submitted by extension users; serves a rudimentary feed for Ads Others Are Seeing tab in extension; serves assets for website.
	- `backend/classifier`: Python scripts for building a model to train a model to predict whether an ad is political given its text, along with scripts for updating each database record with that predictionn.
	- `extension` browser extension.
- [https://github.com/propublica/fbpac-api-public](https://github.com/propublica/fbpac-api-public)
  - `fbpac-api-public` repo Rails app. Serves API for React portion of website (public-facing and admin); manages login for admin dashboard; serves entire Targeting Breakdown page.

Services
---------

### Web servers

- Rust app from `facebook-ads/backend/server`. This runs on Amazon ECS.
- Rails apps from `fbpac-api-public`. This runs on Amazon ECS.
- politicaladcollector.org: simple site showing how to install the ad collector. This is a website run by Github Pages.

### Databases

- a Postgres database. We use an Amazon RDS db.r4.large, but that may be beefier than necessary. However, to my knowledge, it hasn't gotten overloaded.

### Crons
- archiver: data store uploader (English only; This runs daily, with a CloudWatch Event kicking off a the "ecsRunTask" Lambda task that runs an ECS task, as defined in a Dockerfile)
- political classifier, This runs hourly, with a CloudWatch Event kicking off a the "ecsRunTask" Lambda task that runs an ECS task, as defined in a Dockerfile). Also classifies ads by probability of listbuilding/fundraising.
- political model re-builder, This runs daily, with a CloudWatch Event kicking off a the "ecsRunTask" Lambda task that runs a EC2 Spot Instance request, using a custom AMI (with pre-installed dependencies) and a custom UserData script, [train_and_upload_models.sh](backend/classifier/train_and_upload_models.sh)). Trains the political classifier based on votes that have come in.
- fbpac-cache-warmer for recalculating the homepage stats every hour.

### Other infrastructure
 - Amazon S3 folder for storing ad images; the Rust app has to have credentials to write to this bucket.

Portions of the app in need of internationalization:
----------------------------------------------------
 - targeting parser
 - full-text search in Postgres (doesn't handle accents very well, which is a problem for non-English search. Postgres's `unaccent` may be a good option here.)

Maintenance HOWTO:
------------
How to add users to the admin. There's no GUI.
````
$ RAILS_ENV=production
irb > Partner.create!({:email => "whoever@example.com", :password => "whatever", :password_confirmation => "whatever"})
````
