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
from sklearn.gaussian_process import GaussianProcessClassifier
from sklearn.feature_extraction.text import HashingVectorizer
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
from sqlalchemy.sql import text

DB = records.Database()
CLASSIFIER = "MultinomialNB"

def equalize_classes(predictor, response):
    """
    Equalize classes in training data for better representation.
    """
    return SMOTE().fit_sample(predictor, response)

def get_vectorizer(conf):
    """
    Return a HashingVectorizer, which we're using so that we don't need to serialize one.
    """
    return HashingVectorizer(alternate_sign=False, n_features=conf["n_features"])

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
    with open(os.path.join(base, "seeds.json"), 'rb') as json_posts:
        posts = json.load(json_posts)
    data = [(item, 1.0) for item in posts['political']]
    data.extend([(item, 0.0) for item in posts['not_political']])
    print("num seeds: {}".format(len(data)))
    data.extend(load_ads_from_psql(language))
    print("num unique samples: {}".format(len(data)))
    train, test = train_test_split(data)
    x_train, y_train = zip(*train)
    x_test, y_test = zip(*test)
    x_train = vectorizer.transform(x_train)
    x_test = vectorizer.transform(x_test)
    x_train, y_train = equalize_classes(x_train, y_train)
    print("final size of training data: %s" % x_train.shape[0])
    classifier.fit(x_train.todense(), y_train)
    print(classification_report(y_test, classifier.predict(x_test.todense())))
    return classifier

def classifier_path(base):
    """
    Return the path to our serialized classifier
    """
    return os.path.join(base, "classifier.dill")

def get_text(html):
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
        return " ".join([a["href"] for a in doc.find_all('a', href=True) if "facebook.com" in a["href"]])

    return ""

def confs(base):
    """
    Read all the configuration files for our various supported languages.
    """
    for directory in glob(os.path.join(base, "*/")):
        with open(os.path.join(directory, "classifier_config.json"), 'r') as conf:
            yield (directory, json.load(conf))

def load_ads_from_psql(lang):
    """
    Grab ads that users have rated for our classifier
    """
    ads = DB.query("""
      select
        html,
        targeting,
        political::float / (political::float + not_political::float) as score
      from ads
        where lang = '{}'
        and (political + not_political) > 0;
     """.format(text(lang)))

    data = []
    for advert in ads:
        score = round(advert["score"])
        data.append((" ".join([get_text(advert["html"]),
                               get_text(advert["targeting"]),
                               get_profile_links(advert["html"])]), score))
    return data
