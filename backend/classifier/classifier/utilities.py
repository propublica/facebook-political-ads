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
import re
from numpy import matrix
from sklearn.preprocessing import minmax_scale
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
    global d2v_model
    if conf.get("vectorizer_type", None) == "doc2vec":
        print("loading d2v model...")
        d2v_model = load_model("/Users/jm-admin/code/congress2/lib/word2vec/models/model_doc2vec_communications_5_100_0.025_just_member_id_trigrams_dbow.model")

    return (lambda text: HashingVectorizer(
            alternate_sign=False,
            n_features=conf["n_features"],
            ngram_range=(1, 3)
        ).transform(text)) if conf.get("vectorizer_type", None) != "doc2vec" else (lambda texts: minmax_scale(matrix(list(map(
        lambda text: d2v_model.infer_vector(tokenize_words(text)),
        texts)))))


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
    x_train, y_train = zip(*train) # x_train is a tuple of strings.
    x_test, y_test = zip(*test)
    print(type(x_train))
    print(repr(x_train)[0:200])
    print(vectorizer)
    x_train = vectorizer(x_train) # x_train is now a scipy.sparse.csr.csr_matrix; sparse matrix of type numpy.float64
    x_test = vectorizer(x_test)
    print(type(x_train))
    print(repr(x_train)[0:200])
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
                     get_profile_links(advert["html"])])


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


global d2v_model

from gensim.models import Doc2Vec
from tokenizer import TreebankWordTokenizer
treebank_word_tokenizer = TreebankWordTokenizer()
alpha_re = re.compile("^[a-zA-Z]+'?[a-zA-Z]*?$") # allow single apostrophes but not double apostrophes: note, this doesn't allow 'ere
def clean(text):
    text = text.replace("\xa0", " ") # turns NBSPs into regular spaces (was a problem in Del Kilili's statements)
                                     # but we do want to preserve line beraks
    # text = text.encode("ascii","ignore").decode("utf-8") # this is bad, nukes characters from Luján, etc.'s names
    text = text.replace("“", "\"").replace("”", "\"")
    text = re.sub(r"([a-z]+)\.([A-Z][a-z]+)", "\\1. \\2", text)
    text = text.replace("\\r\\n", "\r\n" )
    text = re.sub(r"# ?# ?#", "", text)

    text = re.sub(r"\t[A-Za-z]+( [A-Za-z]+)?\n\t", "", text) # removes one or two words set off by tabs, since they're probably site chrome
    return text
def tokenize_words(text):
    return [s.lower() for s in treebank_word_tokenizer.tokenize(clean(text)) if re.match(alpha_re, s)]


def load_model(args_dot_model):
  if args_dot_model:
    model_name = args_dot_model
  else:
    if not exists(most_recent_model_filename_filename):
      print("Oops! You need to train a model before you can use this function (or specify the location of your model with the --model flag).")
      print("In most cases, running `python train_doc2vec_model.py inputs/your/input/folder` should do the trick!")
      exit(1)
    with open(most_recent_model_filename_filename, "r") as f:
        model_name =  (f.read().strip())
    if not exists(model_name):
      print("Oops! Your model may have been deleted or moved. Either specify its location with the --model flag or fix the path in .texttoolkit_model_filename (or train a new model).")
      exit(1)
  return Doc2Vec.load(model_name)