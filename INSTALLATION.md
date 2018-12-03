Installation: How Build and Develop Locally
===========================================

Here are instructions for building and modifying the components of the Facebook Political Ad Collector on your computer. See [ARCHITECTURE.md](ARCHITECTURE.md) for instructions on how we set it up to run on the web (using Amazon AWS) at ProPublica.

### Extension

The extension popup is a [preact](https://preactjs.com/) application and you can build a development version by running the following:

    $ cd extension
    $ npm install
    $ npm run watch

If you are a Firefox user you can open a clean browser instance with:

    $ npm run ff

and any changes will automatically refresh the extension. (You'll need webpack installed globally.)

In Chrome you can add an unpacked extension by following these [directions](https://developer.chrome.com/extensions/getstarted).

The development version submits ads to the development version of the backend, which is presumed to be running at [localhost:8080](localhost:8080).

### Backend (docker-compose)

For development and evaluation purposes, you can easily set up the FBPAC backend environment using docker-compose. You'll need to clone both this repo and the [fbpac-api-public](https://github.com/propublica/fbpac-api-public) repo to the same parent folder, then run `docker-compose -f docker-compose/docker-compose.yml up` in the root of this directory.s

    $ cd .. 
    $ git clone https://github.com/propublica/fbpac-api-public
    $ cd fbpac-api-public
    $ bundle install
    $ cd ../facebook-political-ads
    $ docker-compose -f docker-compose/docker-compose.yml up

Now visit localhost:8080; you should see the dashboard.

### Running the Backend Natively

#### Running the Rails app (API/auth)
You'll need to clone the [fbpac-api](https://www.github.com/propublica/fbpac-api-public) app and run its migrations, to add a few other columns. In a separate directory:

    $ git clone https://github.com/propublica/fbpac-api-public.git
    $ cd fbpac-api-public
    $ bundle install
    $ rake db:migrate


#### Running the Rust app (website, ad receiver)

The Rust app is this repo.

First, make sure you have Rust [installed](https://doc.rust-lang.org/cargo/getting-started/installation.html):

    $ curl -sSf https://static.rust-lang.org/rustup.sh | sh

Be sure to add `~/.cargo/bin` (or wherever cargo is installed, `path/to/.cargo/bin`) to your PATH. You can do this by adding this line to your `.bash_rc` or `.zshrc` or whatever config file you typically use for your shell.
```
PATH=$PATH:~/.cargo/bin
```

The backend server is a rust application that runs on top of [diesel](https://diesel.rs) and [hyper](https://hyper.rs/). You'll need the diesel command line interface to get started and to create the database:

    $ cargo install diesel_cli
    $ diesel database setup


You can kick the tires by running:

    $ cd backend/server
    $ cargo build
    $ cargo run

This will give a server running at `localhost:8080`. You will also need to build the backend's static resources. To do this, in another terminal tab:

    $ cd backend/server/public
    $ npm install
    $ NODE_ENV=development npm run watch

This will build the required static assets (javascript & css) to view the admin console at `localhost:8080/facebook-ads/`.

If you make any changes to the database, after you run the migration, you'll want to update `the schema with diesel print-schema > src/schema.rs`. You'll need to do this even if you make changes via a Rails migration.

The backend has both unit and integration tests. You will need to set up a test database alongside your actual database in order to run the integration tests. To do this, you will need to the same as above, but substitute out the database test URL:

    $ diesel database setup --database-url postgres://localhost/facebook_ads_test

Note that the value for `--database-url` came from the `TEST_DATABASE_URL` value set in the `.env` file. Make sure that the urls match before you run the tests!

Additionally, because the integration tests use the database, we want to make sure that they aren't run in parallel, so to run the tests:

    $ RUST_TEST_THREADS=1 cargo test

This will run the tests in sequence, avoiding parallelism errors for tests that require the database.

### Classifier

We train the classifier using python and scikit learn and the source is in `backend/classifier/`. We're using [pipenv](https://docs.pipenv.org/) to track dependencies. 

To download pipenv:
```
$ brew install pipenv
```

To get started you can run:

    $ cd backend/classifier/
    $ pipenv install
    $ pipenv shell

To download the seeds for the classifier, you'll need a Facebook app with the proper permissions and you'll run the seed command like this:

    $ FACEBOOK_APP_ID=whatever FACEBOOK_APP_SECRET=whatever DATABASE_URL=postgres://whatever/facebook_ads ./classify seed en-US`

Alternatively, you can build the model without seeds, relying instead just on votes in the extension and suppressions in the admin. And to build the classifier you'll want to run:

    $ pipenv run ./classify build

To classify the ads you've collected you can run:

    $ pipenv run ./classify classify
    
You can download pre-trained models with `pipenv run ./classify get_models`.

### Internationalization and Localization

The FBPAC system -- user-facing extension, backend and classifiers -- are all built to enable internationalization. Here are the steps you'd need to take to make FBPAC work in your country.

##### Translate the extension

Translations for the extension are stored in `extension/_locales/${locale}/messages.json`.

A `locale` is a ISO 639-1 language code (e.g. `en`, `de`) with an optional ISO 3166-1 Alpha-2 country suffix (e.g. `de_CH`).

Users can select from all known languages and countries while onboarding. The UI then uses the first available translation in following order: `${langauge}_${country}`, `${langauge}`, `en`.

In `extension/src/i18n.js` a list of active language and country codes can be defined. Active ones get prioritised in the UI.


##### Add your country's translation of "Sponsored"

The FBPAC extension detects advertisements on Facebook using the "Sponsored" tag that appears on each ad. But, this "Sponsored" tag is translated, e.g. to "Commandité" or "Sponsorisé". Find the list of translations around line 585 of [extension/src/parser.js](extension/src/parser.js) and, if it's not present, add it.

##### Training a model

The FBPAC system uses a machine-learning model to classify whether each ad is political or not. We will need to tell the system to create one such machine-learning model for your language. You'll need to create a folder for your language/country code in `backend/classifier/data/`. Then copy the contents of the (blank) Irish English classifier in `backend/classifier/data/en-IE` and replace `en-IE` in `classifier-config.josn` with your language code. Finally, add your language code to the list in `backend/classifier/train_and_upload_models.sh`. 

That'll get a model trained... but... we still don't have any data to train it on. Unless you have a great idea on how to get a list of political Facebook posts and a list of non-political Facebook posts from your country (maybe Twitter posts would work?), you'll be starting from scratch. That means you and your users will have to install the extension, visit Facebook, collect ads, then rate those ads as political or not-political in the popup window of the extension. Gradually, once you've collected and rated enough ads, the classifier will begin to do a good job.

##### Accounting for your country's language and election structure in the admin

There are two major assumptions in the backend that you may have to adjust for your country.

1.) Search. Search uses Postgresql's full text search system, using `tsvectors`. You may have to install additional postgres modules (e.g. `unaccent`) to get search to work properly in your language.

2.) Electoral structure. The admin contains USA-specific assumptions about ads, like joining ads (via the `page_url` attribute) to candidates, then letting users see ads just from that one political party or just one state. Or seeing ads that [either mention a state, mention a candidate in a state or are from a candidate in that state](https://github.com/propublica/fbpac-api-public/blob/master/app/controllers/ads_controller.rb#L251-L288). Much of this won't make sense in your country (maybe there are no political parties?), so if you want to use tools like this

##### Targeting parser

This one's hard. 

Facebook provides a brief, incomplete explanation of how an ad was targeted to the user who saw it. [backend/server/src/targeting_parser.rs](backend/server/src/targeting_parser.rs) parses that explanation into attributes the computer can understaand. It might look something like this:

_One reason you're seeing this ad is that Donald J. Trump added you to a list of people they want to reach on Facebook. They were able to reach you because you're on a customer list collected by Donald J. Trump or its partners, or you've provided them with your information off of Facebook.
There may be other reasons you're seeing this ad, including that Donald J. Trump wants to reach people ages 18 and older who live in the United States. This is information based on your Facebook profile and where you've connected to the internet._

and [targeting_parser.rs](backend/server/src/targeting_parser.rs) parses thaat into `Age: 18 and older, MinAge: 18, Region: the United States, List`. This enables you to, for instance, find all ads targeted to a list of people.

These explanations are often presented in your own language. If you want to parse them, you'll have to modify the parser code in [backend/server/src/targeting_parser.rs](backend/server/src/targeting_parser.rs). This is hard! Use the English and the German examples as a guide; it'll probably require a lot of trial and error, along with gathering a wide variety of those targeting descriptions. I don't know an easy way to do it. (And our parser isn't _quite_ perfect even in English.)


##### Locale Specific Styles in Extension Popup

You can customize, for example font sizes, with `[lang]` and `[data-locale]` CSS selectors:

```css
[lang=de] .toggle {
  font-size: 0.78rem;
}
[data-locale=de_CH] .toggle {
  font-size: 0.78rem;
}
```
