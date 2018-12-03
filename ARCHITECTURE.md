Architecture
============

The Facebook Political Ad Collector has several pieces. This document describes the various services required to run a version of FBPAC on your own, and how those services map to bits of code.

INSTALLATION.md 

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

- Rust app from `facebook-ads/backend/server`.
- Rails apps from `fbpac-api-public`.
- politicaladcollector.org

### Databases

- a Postgres database. We use an Amazon RDS db.r4.large, but that may be beefier than necessary. However, to my knowledge, it hasn't gotten overloaded.

### Crons
- archiver: data store uploader (English only; daily, ecsRunTask -> Dockerfile)
- political classifier, hourly (ecsRunTask -> Dockerfile fbpac-classifier). Also classifies ads by probability of listbuilding/fundraising.
- political model re-builder, weekly (separate Lambda). Trains the political classifier based on votes that have come in.
- fbpac-cache-warmer for recalculating the homepage stats every hour.

### Other infrastructure
 - Amazon S3 for storing ad images.

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
