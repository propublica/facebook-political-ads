# Facebook Political Ad Collector

This is the source code behind our project to collect political ads on Facebook.

We're asking our readers to use this extension when they are browsing Facebook. While they are on a background script to collect ads they see. The extension shows those ads to users and asks them to decide whether or not a particular ad is political. We use those ratings to train a naive bayes classifier that then automatically rates the other ads we collected. The extension asks the server for the most recent ads that we've classified as political so that users can see political ads they haven't seen. We're careful to protect our user's privacy by not sending identifying information to our backend server.

We're open sourcing this project because we'd love your help. Collecting these ads is challenging, and the more eyes on the problem the better.

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

    cd backend
    cargo build
    cargo run --bin server -p server

We train the classifier using python and scikit learn and the source is in `backend/classifier/`. We're using [pipenv](http://docs.pipenv.org/en/latest/) to track dependencies. To get started you can run:

    cd backend/classifier/
    pipenv install
    pipenv shell

And to run the classifier you'll want to run:

    cd src/
    python build_classifier.py classifier.conf

To classify the ads you've collected you can run:

    cargo run --bin classify -p server <lang> <path-to-classifier.json>

So for example, to classify ads in the United States I'd run:

    cargo run --bin classify -p server en-US data/en_us/seeds.json

## Where We Need Your Help

In general, the project needs more tests. We've written a couple of tests for parsing the Facebook timeline in the extension directory, and a few for the tricky bits in the server, but any help here would be great!

Also, the rust backend needs a bit of love and care, and there is a bit of a mess in `backend\server\src\server.s` that could use cleaning up. It would be nice to move the classification to rust as well.
