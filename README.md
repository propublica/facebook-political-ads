# Facebook Political Ad Collector

This is the source code behind our project to collect political ads on Facebook. Built versions are available for [Firefox](https://addons.mozilla.org/en-US/firefox/addon/facebook-ad-collector/) and [Chrome](https://chrome.google.com/webstore/detail/facebook-political-ad-col/enliecaalhkhhihcmnbjfmmjkljlcinl).

We're asking our readers to use this extension when they are browsing Facebook. While they are on Facebook a background script runs to collect ads they see. The extension shows those ads to users and asks them to decide whether or not a particular ad is political. Serverside, we use those ratings to train a naive bayes classifier that then automatically rates the other ads we've collected. The extension also asks the server for the most recent ads that the classifier thinks are political so that users can see political ads they haven't seen. We're careful to protect our user's privacy by not sending identifying information to our backend server.

We're open sourcing this project because we'd love your help. Collecting these ads is challenging, and the more eyes on the problem the better.

## Download and Try It

* Download the [Facebook Political Ad Collector for Firefox](https://addons.mozilla.org/en-US/firefox/addon/facebook-ad-collector/)
* Download the [Facebook Political Ad Collector for Chrome](https://chrome.google.com/webstore/detail/facebook-political-ad-col/enliecaalhkhhihcmnbjfmmjkljlcinl?hl=en)

## Build and Develop Locally

The extension popup is a [preact](https://preactjs.com/) application and you can build a development version by running the following:

    cd extension
    npm install
    npm watch

If you are a Firefox user you can open a clean browser instance with:

    npm run ff

and any changes will automatically refresh the extension.

In Chrome you'll need to add an unpacked extension by following these [directions](https://developer.chrome.com/extensions/getstarted).

The backend server is a rust application that runs on top of [diesel](https://diesel.rs) and [hyper](https://hyper.rs/). You'll need the diesel command line interface to get started and to create the database:

    cargo install diesel_cli
    diesel database setup

You can kick the tires by running:

    cd backend/server
    cargo build
    cargo run

We train the classifier using python and scikit learn and the source is in `backend/classifier/`. We're using [pipenv](http://docs.pipenv.org/en/latest/) to track dependencies. To get started you can run:

    cd backend/classifier/
    pipenv install
    pipenv shell

And to build the classifier you'll want to run:

    ./classify build

To classify the ads you've collected you can run:

    ./classify classify

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


## Where We Need Your Help

In general, the project needs more tests. We've written a couple of tests for parsing the Facebook timeline in the extension directory, and a few for the tricky bits in the server, but any help here would be great!

Also, the rust backend needs a bit of love and care, and there is a bit of a mess in `backend/server/src/server.rs` that could use cleaning up.
