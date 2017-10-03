"""
Various utilities and general helper functions.
"""
import dill
from glob import glob
import json
import os
import records
from bs4 import BeautifulSoup
from sklearn.naive_bayes import MultinomialNB, BernoulliNB
from sklearn.linear_model import LogisticRegression
from sklearn.feature_extraction.text import HashingVectorizer
from imblearn.over_sampling import SMOTE

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

def classifier_path(base):
    """
    Return the path to our serialized classifier
    """
    return os.path.join(base, "classifier.dill")

def get_classifiers():
    """
    Return a dict of the classifiers we currently support
    """
    return {
        "MultinomialNB" : MultinomialNB(),
        "BernoulliNB" : BernoulliNB(),
        "LogisticRegression" : LogisticRegression()
    }

def get_text(html):
    """
    Return the raw text of an ad
    """
    doc = BeautifulSoup(html, "html.parser")
    return doc.get_text(" ")

def confs(base):
    """
    Read all the configuration files for our various supported languages.
    """
    for directory in glob(os.path.join(base, "*/")):
        with open(os.path.join(directory, "classifier_config.json"), 'r') as conf:
            yield (directory, json.load(conf))
