"""
Various utilities and general helper functions.
"""
from glob import glob
import json
import os
from bs4 import BeautifulSoup
from imblearn.over_sampling import SMOTE
import records
from sklearn.naive_bayes import MultinomialNB
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_extraction.text import HashingVectorizer
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
from sqlalchemy.sql import text

DB = records.Database()
CLASSIFIER = "MultinomialNB"


def equalize_classes(predictor, response):
    """
    Equalize classes in training data for better representation.

    https://en.wikipedia.org/wiki/Oversampling_and_undersampling_in_data_analysis#SMOTE
    """
    return SMOTE().fit_sample(predictor, response)


def get_vectorizer(conf):
    """
    Return a HashingVectorizer, which we're using so that we don't
    need to serialize one.
    """
    return HashingVectorizer(
        alternate_sign=False,
        n_features=conf["n_features"],
        ngram_range=(1, 3)
    )


def get_classifier():
    """
    Return a classifier instance
    """
    return get_classifiers()[CLASSIFIER]


def get_classifiers():
    """
    Return a dict of the classifiers we currently support
    """
    return {
        "MultinomialNB": MultinomialNB(),
        "LogisticRegression": LogisticRegression(),
        "RandomForest": RandomForestClassifier()
    }


def train_classifier(classifier, vectorizer, base, language):
    """
    Train a classifier with a given vectorizer, seeds, and language
    """
    print("============ {} =============".format(base))
    with open(os.path.join(base, "seeds.json"), 'r') as json_posts:
        posts = json.load(json_posts)
    data = [(item, 1.0) for item in posts['political']]
    data.extend([(item, 0.0) for item in posts['not_political']])
    print("num seeds: {}".format(len(data)))
    data.extend(load_ads_from_psql(language))
    print("num unique samples: {}".format(len(data)))
    train, test = train_test_split(data, test_size=0.1)
    x_train, y_train = zip(*train)
    x_test, y_test = zip(*test)
    x_train = vectorizer.transform(x_train)
    x_test = vectorizer.transform(x_test)
    x_train, y_train = equalize_classes(x_train, y_train)
    print("final size of training data: %s" % x_train.shape[0])
    classifier.fit(x_train, y_train)
    print(classification_report(y_test, classifier.predict(x_test)))
    return classifier


def classifier_path(base):
    """
    Return the path to our serialized classifier
    """
    return os.path.join(base, "classifier.dill")


def get_html_text(html):
    """
    Return the raw text of an ad
    """
    if html:
        doc = BeautifulSoup(html, "html.parser")
        return doc.get_text(" ")

    return ""


def get_targets(html):
    """
    Return bolded targeting parameters
    """
    if html:
        doc = BeautifulSoup(html, "html.parser")
        return " ".join([b.get_text(" ") for b in doc.find_all("b")])

    return ""


def get_profile_links(html):
    """
    Return the links in an ad.
    """
    if html:
        doc = BeautifulSoup(html, "html.parser")
        return " ".join([a["href"] for a in
                         doc.find_all('a', href=True)
                         if "facebook.com" in a["href"]])

    return ""


def get_text(advert):
    """
    Return the features we're using to classify the text.
    """
    return " ".join([get_html_text(advert["html"]),
                     get_targets(advert["targeting"]),
                     get_profile_links(advert["html"])]).replace("triangle-down triangle-up Like share Share It looks like you may be having problems playing this video. If so, please try restarting your browser. Close", '')


def confs(base):
    """
    Read all the configuration files for our various supported languages.
    """
    for directory in glob(os.path.join(base, "*/")):
        config = os.path.join(directory, "classifier_config.json")
        with open(config, 'r') as conf:
            yield (directory, json.load(conf))


def entities_confs(base):
    """
    Read all the entity configuration files for our various supported
    languages.
    """
    for directory in glob(os.path.join(base, "*/")):
        if os.path.isfile(os.path.join(directory, "entities_config.json")):
            config = os.path.join(directory, "entities_config.json")
            with open(config, 'r') as conf:
                yield (directory, json.load(conf))
        else:
            yield (directory, False)


def load_ads_from_psql(lang):
    """
    Grab ads that users have rated for our classifier
    """
    ads = DB.query("""
      select
        html,
        targeting,
        political::float / ((political::float + not_political::float) + 0.01) as score,
        suppressed
      from ads
        where lang = '{}'
        and ((political + not_political) > 0 or suppressed = true)
      limit 100000;
     """.format(text(lang)))

    data = []
    for advert in ads:
        if advert['suppressed']:
            score = 0
        else:
            score = round(advert["score"])
        data.append((get_text(advert), score))
    return data
