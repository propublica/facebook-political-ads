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

from bs4 import BeautifulSoup

from sklearn.naive_bayes import MultinomialNB, BernoulliNB, GaussianNB
from sklearn.linear_model import LogisticRegression
from sklearn.feature_extraction.text import HashingVectorizer

from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score, classification_report

CLASSIFIERS = {
    "MultinomialNB" : MultinomialNB(),
    "BernoulliNB" : BernoulliNB(),
    "GaussianNB" : GaussianNB(),
    "LogisticRegression" : LogisticRegression()
}

def equalize_classes(train):
    """Equalize classes in training data for better representation.

    Currently, just repeat samples from underrepresented class until
    both are equal.

    TODO: In future use SMOTE to equalize?
    """
    class_diff = len([x for x in train if x[1] == 0]) - \
                 len([x for x in train if x[1] == 1])
    if class_diff != 0:
        to_append = [x for x in train if x[1] == (1 if class_diff > 0 else 0)]
        for i in range(class_diff):
            train.append(to_append[i % len(to_append)])

    return train

def eval_classifiers(X_train, Y_train, X_test, Y_test):
    """Compare available classifiers, print and plot results.

    Currently supported:
    MultinomialNB, BernoulliNB, GaussianNB, LogisticRegression
    """
    for name, classifier in CLASSIFIERS.items():
        classifier.fit(X_train.todense(), Y_train)
        preds = classifier.predict_proba(X_test.todense())[:, 1]
        print("******** %s: %s" % (name, roc_auc_score(Y_test, preds)))
        preds = classifier.predict(X_test.todense())
        print(classification_report(Y_test, preds))

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('No config file provided!')
        print('Usage: python run_classifier.py [config_filename]')
        exit()

    with open(sys.argv[1], 'r') as f:
        config = json.load(f)

    with open(config['input_filename']) as f:
        posts = json.load(f)

    pol = [(item, 1) for item in posts['political']]
    npol = [(item, 0) for item in posts['not_political']]
    data = pol + npol
    if config["read_from_psql"]:
        conn = psycopg2.connect(os.environ["DATABASE_URL"])
        cur = conn.cursor()
        cur.execute("""
          select
            html,
            political::float / (political::float + not_political::float) as political
          from ads
            where lang = %s
            and (political + not_political) > 0;
         """, (config["language"], ))
        for row in cur:
            html, score = row
            doc = BeautifulSoup(html, "html.parser")
            if score > 0.5:
                score = 1.0
            else:
                score = 0.0
            data.append((doc.get_text(), score))

    train, test = train_test_split(data)

    if config['equalize_classes']:
        train = equalize_classes(train)

    X_train, Y_train = zip(*train)
    X_test, Y_test = zip(*test)

    vectorizer = HashingVectorizer(alternate_sign=False, n_features=config['n_features'])
    X_train = vectorizer.transform(X_train)
    X_test = vectorizer.transform(X_test)

    if config['run_eval']:
        eval_classifiers(X_train, Y_train, X_test, Y_test)

    if not config['classifier_type']:
        print('Need to specify model class.')
        print('Currently supported:')
        print('MultinomialNB, BernoulliNB, GaussianNB, LogisticRegression')
        exit()

    classifier = CLASSIFIERS[config['classifier_type']]
    classifier.fit(X_train.todense(), Y_train)
    preds = classifier.predict(X_test.todense())
    print(classification_report(Y_test, preds))

    model = {}
    model['feature_log_prob'] = list(itertools.chain(*classifier.feature_log_prob_.tolist()))
    model['class_log_prior'] = classifier.class_log_prior_.tolist()
    model['n_features'] = config['n_features']
    model['n_classes'] = 2

    model_filename = config['output_filename']
    with open(model_filename, 'w') as f:
        json.dump(model, f)
    print('Dumped model to file ' + model_filename)
