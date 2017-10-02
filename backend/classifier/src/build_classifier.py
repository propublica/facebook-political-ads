"""build_classifier.py

Usage: python build_classifier.py [config_file]

Builds and writes model for classifying political vs. not-political.

Usage: python build_classifier.py [EVAL|BUILD|CLASSIFY] [config_filename]
Supported modes:
    EVAL: Compare classifier performance
    BUILD: Create model and write to model file specified in config
    CLASSIFY: Write results of prediction to psql at env[DATABASE_URL]
              using model from model_filename
"""
import dill
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
    with open(filename, 'wb') as f:
        dill.dump(clf, f)
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

def write_predictions_to_psql(database_url, lang, model_filename, vectorizer):
    with open(model_filename, 'rb') as f:
        clf = dill.load(f)

    conn = psycopg2.connect(database_url)
    cur = conn.cursor('select-cursor', withhold=True)
    cur.itersize = 1000000

    updatecur = conn.cursor()

    cur.execute("select id, html from ads where lang = %s", (lang,))

    cnt = 0
    for pid, html in cur:
        cnt += 1
        doc = BeautifulSoup(html, "html.parser")
        text = doc.get_text(" ")
        transformed_text = vectorizer.transform([text])
        pol_score = clf.predict_proba(transformed_text)[0][1]
        updatecur.execute("UPDATE ads SET political_probability=%s WHERE id=%s", (pol_score, pid))
        print(str(cnt) + ' ads processed out of ' + str(cur.rowcount))

        if cnt % 1000 == 0:
            conn.commit()

    conn.commit()
    conn.close()

if __name__ == '__main__':
    if len(sys.argv) < 3 or sys.argv[1] not in ['EVAL', 'BUILD', 'CLASSIFY']:
        print('No config file or mode provided!')
        print('Usage: python build_classifier.py [EVAL|BUILD|CLASSIFY] [config_filename]')
        print('Supported modes:')
        print('EVAL: Compare classifier performance')
        print('BUILD: Create model and write to model file specified in config')
        print('CLASSIFY: Write results of prediction to psql at env[DATABASE_URL],')
        print(' using model from model_filename')
        exit()

    with open(sys.argv[2], 'r') as f:
        config = json.load(f)

    model_filename = config['model_filename']

    if sys.argv[1] == 'CLASSIFY':
        vectorizer = HashingVectorizer(alternate_sign=False, n_features=config['n_features'])
        write_predictions_to_psql(os.environ["DATABASE_URL"], config["language"],
                                  model_filename, vectorizer)
        exit()

    with open(config['seed_filename']) as f:
        posts = json.load(f)

    data = [(item, 1.0) for item in posts['political']]
    print("seed length: %s" % len(data))
    if config["read_from_psql"]:
        data.extend(load_ads_from_psql(os.environ["DATABASE_URL"], config["language"]))

    print("num unique samples: %s" % len(data))
    train, test = train_test_split(data)
    X_train, Y_train = zip(*train)
    X_test, Y_test = zip(*test)

    vectorizer = HashingVectorizer(alternate_sign=False, n_features=config['n_features'])
    X_train = vectorizer.transform(X_train)
    X_test = vectorizer.transform(X_test)
    X_train, Y_train = equalize_classes(X_train, Y_train)

    print("final size of training data: %s" % X_train.shape[0])

    if sys.argv[1] == 'EVAL':
        eval_classifiers(X_train, Y_train, X_test, Y_test)
        exit()

    if sys.argv[1] == 'BUILD':
        if not config['classifier_type']:
            print('Need to specify model class.')
            print('Currently supported:')
            print('MultinomialNB, BernoulliNB, Logisticregression')
            exit()

        classifier = CLASSIFIERS[config['classifier_type']]
        classifier.fit(X_train.todense(), Y_train)
        preds = classifier.predict(X_test.todense())
        print(classification_report(Y_test, preds))

        dump_classifier(model_filename, classifier)
        exit()
