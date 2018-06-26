# Facebook Political Ad Collector

This is the source code behind our project to collect political ads on Facebook. Built versions are available for [Firefox](https://addons.mozilla.org/en-US/firefox/addon/facebook-ad-collector/) and [Chrome](https://chrome.google.com/webstore/detail/facebook-political-ad-col/enliecaalhkhhihcmnbjfmmjkljlcinl). You can browse the American ads we've collected at [ProPublica](http://projects.propublica.org/facebook-ads/), and the Australian ads over on the [Guardian's website](https://www.theguardian.com/technology/ng-interactive/2017/oct/25/revealed-how-australians-are-targeted-with-political-advertising-on-facebook).

We're asking our readers to use this extension when they are browsing Facebook. While they are on Facebook a background script runs to collect ads they see. The extension shows those ads to users and asks them to decide whether or not a particular ad is political. Serverside, we use those ratings to train a naive bayes classifier that then automatically rates the other ads we've collected. The extension also asks the server for the most recent ads that the classifier thinks are political so that users can see political ads they haven't seen. We're careful to protect our user's privacy by not sending identifying information to our backend server.

We're open sourcing this project because we'd love your help. Collecting these ads is challenging, and the more eyes on the problem the better.

## Download and Try It

* Download the [Facebook Political Ad Collector for Firefox](https://addons.mozilla.org/en-US/firefox/addon/facebook-ad-collector/)
* Download the [Facebook Political Ad Collector for Chrome](https://chrome.google.com/webstore/detail/facebook-political-ad-col/enliecaalhkhhihcmnbjfmmjkljlcinl?hl=en)

## Build and Develop Locally

### Extension

The extension popup is a [preact](https://preactjs.com/) application and you can build a development version by running the following:

    cd extension
    npm install
    npm run watch

If you are a Firefox user you can open a clean browser instance with:

    npm run ff

and any changes will automatically refresh the extension. (You'll need webpack installed globally.)

In Chrome you'll need to add an unpacked extension by following these [directions](https://developer.chrome.com/extensions/getstarted).

### Backend

The backend server is a rust application that runs on top of [diesel](https://diesel.rs) and [hyper](https://hyper.rs/). You'll need the diesel command line interface to get started and to create the database:

    cargo install diesel_cli
    diesel database setup
    
You'll also need to clone the [fbpac-api](https://www.github.com/propublica/fbpac-api-public) app and run its migrations, to add a few other columns.

    rake db:migrate

You can kick the tires by running:

    cd backend/server
    cargo build
    cargo run

This will give a server running at `localhost:8080`. You will also need to build the backend's static resources. To do this, in another terminal tab:

    cd backend/server/public
    npm install
    npm run watch

This will build the required static assets (javascript & css) to view the admin console at `localhost:8080/facebook-ads/`.

If you make any changes to the database, after you run the migration, you'll want to update `the schema with diesel print-schema > src/schema.rs`. You'll need to do this even if you make changes via a Rails migration.

The backend has both unit and integration tests. You will need to set up a test database alongside your actual database in order to run the integration tests. To do this, you will need to the same as above, but substitute out the database test URL:

    diesel database setup --database-url postgres://localhost/facebook_ads_test

Note that the value for `--database-url` came from the `TEST_DATABASE_URL` value set in the `.env` file. Make sure that the urls match before you run the tests!

Additionally, because the integration tests use the database, we want to make sure that they aren't run in parallel, so to run the tests:

    RUST_TEST_THREADS=1 cargo test

This will run the tests in sequence, avoiding parallelism errors for tests that require the database.

### Classifier

We train the classifier using python and scikit learn and the source is in `backend/classifier/`. We're using [pipenv](http://docs.pipenv.org/en/latest/) to track dependencies. To get started you can run:

    cd backend/classifier/
    pipenv install
    pipenv shell

To download the seeds for the classifier, you'll need a Facebook app with the proper permissions and you'll run the seed command like this:

    FACEBOOK_APP_ID=whatever FACEBOOK_APP_SECRET=whatever DATABASE_URL=postgres://whatever/facebook_ads ./classify seed en-US`

Alternatively, you can build the model without seeds, relying instead just on votes in the extension and suppressions in the admin. And to build the classifier you'll want to run:

    pipenv run ./classify build

To classify the ads you've collected you can run:

    pipenv run ./classify classify
    
You can download pre-trained models with `pipenv run ./classify get_models`.

### Internationalization and Localization

Translations for the extension are stored in `extension/_locales/${locale}/messages.json`.

A `locale` is a ISO 639-1 language code (e.g. `en`, `de`) with an optional ISO 3166-1 Alpha-2 country suffix (e.g. `de_CH`).

Users can select from all known languages and countries while onboarding. The UI then uses the first available translation in following order: `${langauge}_${country}`, `${langauge}`, `en`.

#### Active Languages and Countries

In `extension/src/i18n.js` a list of active language and country codes can be defined. Active ones get prioritised in the UI.

#### Locale Specific Styles in Popup

You can customize, for example font sizes, with `[lang]` and `[data-locale]` CSS selectors:

```css
[lang=de] .toggle {
  font-size: 0.78rem;
}
[data-locale=de_CH] .toggle {
  font-size: 0.78rem;
}
```

## Stories

* [Help Us Monitor Political Ads Online](https://www.propublica.org/article/help-us-monitor-political-ads-online)
* [Mehr Transparenz im Schatten-Wahlkampf](http://faktenfinder.tagesschau.de/wahlkampf-facebook-dark-ads-101.html)
* [Bringen Sie Licht in den dunklen Facebook-Wahlkampf](http://www.sueddeutsche.de/digital/bundestagswahl-bringen-sie-licht-in-den-dunklen-facebook-wahlkampf-1.3656582)
* [Wie werben die Parteien auf Facebook?](http://www.spiegel.de/netzwelt/games/facebook-political-ad-collector-plugin-sammelt-wahlwerbung-auf-facebook-ein-a-1166566.html)
* [So werben die Parteien auf Facebook](http://www.spiegel.de/netzwelt/web/facebook-political-ad-collector-parteienwerbung-auf-facebook-im-ueberblick-a-1169154.html)
* [Warum Zuckerberg den deutschen Wahlkampf durchleuchten ließ ](http://www.sueddeutsche.de/digital/werbung-auf-facebook-und-google-warum-zuckerberg-den-deutschen-wahlkampf-durchleuchten-liess-1.3679603)
* [Trust Issues](https://www.wnyc.org/story/on-the-media-2017-09-22/)
* [Facebook Allowed Questionable Ads in German Election Despite Warnings](https://www.propublica.org/article/facebook-allowed-questionable-ads-in-german-election-despite-warnings)
* [Versuch der anonymen Einflussnahme auf den Bundestagswahlkampf](http://www.sueddeutsche.de/digital/facebook-versuch-der-anonymen-einflussnahme-auf-den-bundestagswahlkampf-1.3713694)
* [Bundestagswahl: Versuch anonymer Einflussnahme](https://www.ndr.de/fernsehen/sendungen/zapp/Greenwatch-Versuch-anonymer-Einflussnahme,greenwatch100.html)
* [Same-sex marriage survey: help us track targeted ads on Facebook ](https://www.theguardian.com/australia-news/2017/oct/17/same-sex-marriage-survey-help-track-targeted-ads-facebook)
* [Revealed: how Australians are targeted with political advertising on Facebook (Searchable Database)](https://www.theguardian.com/technology/ng-interactive/2017/oct/25/revealed-how-australians-are-targeted-with-political-advertising-on-facebook)
* [Adani posts weird video ad on Facebook to fend off Carmichael criticism](https://www.theguardian.com/business/2017/oct/21/adani-posts-weird-video-ad-on-facebook-to-fend-off-carmichael-criticism)
* [How Malcolm Turnbull, GetUp and Adani are using Facebook ads to push their agenda](https://www.theguardian.com/technology/2017/oct/25/how-malcolm-turnbull-getup-and-adani-are-using-facebook-ads-to-push-their-agenda)
* [Hjælp os med at kortlægge politiske reklamer på Facebook](https://www.information.dk/indland/2017/11/hjaelp-kortlaegge-politiske-reklamer-paa-facebook)
* [Facebook Allowed Political Ads That Were Actually Scams and Malware](https://www.propublica.org/article/facebook-political-ads-malware-scams-misleading)
* [Political Ads on Facebook](http://projects.propublica.org/facebook-ads/)
* [Helfen Sie uns, verdeckte Polit-Werbung zu enttarnen](https://www.republik.ch/updates/polit-werbung-enttarnen)


## Where We Need Your Help

In general, the project needs more tests. We've written a couple of tests for parsing the Facebook timeline in the extension directory, and a few for the tricky bits in the server, but any help here would be great!

Also, the rust backend needs a bit of love and care, and there is a bit of a mess in `backend/server/src/server.rs` that could use cleaning up.
