"""build_classifier.py

Usage: python build_classifier.py [config_file]

Builds and writes model for classifying political vs. not-political.

When run with "run_eval" option, compares performance of
several different models.
"""
import itertools
import json
import os
import sys
import psycopg2
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

from bs4 import BeautifulSoup

from sklearn.naive_bayes import MultinomialNB, BernoulliNB
from sklearn.linear_model import LogisticRegression
from sklearn.feature_extraction.text import HashingVectorizer

from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score, classification_report, roc_curve, brier_score_loss

from imblearn.over_sampling import SMOTE

CLASSIFIERS = {
    "MultinomialNB" : MultinomialNB(),
    "BernoulliNB" : BernoulliNB(),
    "LogisticRegression" : LogisticRegression()
}

def equalize_classes(x, y):
    """Equalize classes in training data for better representation.

    """
    return SMOTE().fit_sample(x, y)

def eval_classifiers(X_train, Y_train, X_test, Y_test):
    """Compare available classifiers, print and plot results.

    Currently supported:
    MultinomialNB, BernoulliNB, GaussianNB, LogisticRegression
    """
    for name, classifier in CLASSIFIERS.items():
        classifier.fit(X_train.todense(), Y_train)
        preds = classifier.predict_proba(X_test.todense())[:, 1]
        print("******** %s: %s" % (name, roc_auc_score(Y_test, preds)))
        fpr, tpr, _ = roc_curve(Y_test, preds)
        preds = classifier.predict(X_test.todense())
        print(classification_report(Y_test, preds))
        print(brier_score_loss(Y_test, preds))
        plt.plot(fpr, tpr, label=name)
    plt.xlabel("FPR")
    plt.ylabel("TPR")
    plt.legend()
    plt.savefig("out.png")

def dump_classifier(filename, clf):
    """Write classifier out to disk."""
    model = {}
    model['feature_log_prob'] = list(itertools.chain(*classifier.feature_log_prob_.tolist()))
    model['class_log_prior'] = classifier.class_log_prior_.tolist()
    model['n_features'] = config['n_features']
    model['n_classes'] = 2
    
    with open(filename, 'w') as f:
        json.dump(model, f)
    print('Dumped model to file ' + filename)

def load_ads_from_psql(database_url, lang):
    conn = psycopg2.connect(database_url)
    cur = conn.cursor()
    cur.execute("""
      select
        html,
        political::float / (political::float + not_political::float) as political
      from ads
        where lang = %s
        and (political + not_political) > 0;
     """, (lang, ))

    data = []
    for html, score in cur:           
        doc = BeautifulSoup(html, "html.parser")
        if score > 0.5:
            score = 1.0
        else:
            score = 0.0
        data.append((doc.get_text(" "), score))
    return data

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('No config file provided!')
        print('Usage: python build_classifier.py [config_filename]')
        exit()

    with open(sys.argv[1], 'r') as f:
        config = json.load(f)

    with open(config['input_filename']) as f:
        posts = json.load(f)

    data = [(item, 1.0) for item in posts['political']]
    print("seed length %s" % len(data))
    if config["read_from_psql"]:
        data.extend(load_ads_from_psql(os.environ["DATABASE_URL"], config["language"]))

    print("With data %s" % len(data))
    train, test = train_test_split(data)
    X_train, Y_train = zip(*train)
    X_test, Y_test = zip(*test)

    vectorizer = HashingVectorizer(alternate_sign=False, n_features=config['n_features'])
    X_train = vectorizer.transform(X_train)
    X_test = vectorizer.transform(X_test)
    X_train, Y_train = equalize_classes(X_train, Y_train)

    print("Final size: %s" % X_train.shape[0])

    if config['run_eval']:
        eval_classifiers(X_train, Y_train, X_test, Y_test)

    if not config['classifier_type']:
        print('Need to specify model class.')
        print('Currently supported:')
        print('MultinomialNB, BernoulliNB, Logisticregression')
        exit()

    classifier = CLASSIFIERS[config['classifier_type']]
    classifier.fit(X_train.todense(), Y_train)
    preds = classifier.predict(X_test.todense())
    print(classification_report(Y_test, preds))

    model_filename = config['output_filename']
    dump_classifier(model_filename, classifier)
