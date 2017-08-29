"""build_classifier.py

Usage: python build_classifier.py [config_file]

Builds and writes model for classifying political vs. not-political.

When run with "run_eval" option, compares performance of
several different models.
"""
import json
import sys

from sklearn.naive_bayes import MultinomialNB,BernoulliNB,GaussianNB
from sklearn.linear_model import LogisticRegression
from sklearn.feature_extraction.text import HashingVectorizer

from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.metrics import roc_curve, roc_auc_score, classification_report
import matplotlib.pyplot as plt

classifiers = {
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
    for name, classifier in classifiers.items():
        classifier.fit(X_train.todense(), Y_train)
        preds = classifier.predict_proba(X_test.todense())[:, 1]
        fpr, tpr, _ = roc_curve(Y_test, preds)
        print("******** %s: %s" % (name, roc_auc_score(Y_test, preds)))
        plt.plot(fpr, tpr, label=name)
        preds = classifier.predict(X_test.todense())
        print(classification_report(Y_test, preds))
    plt.xlabel('FPR')
    plt.ylabel('TPR')
    plt.legend()
    plt.show()

    return

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

    train, test = train_test_split(data, test_size=0.5)

    if config['equalize_classes']:
        train = equalize_classes(train)

    X_train, Y_train = zip(*train)
    X_test, Y_test = zip(*test)

    vectorizer = HashingVectorizer(alternate_sign=False, n_features=config['n_features'])
    X_train = vectorizer.transform(X_train)
    X_test = vectorizer.transform(X_test)

    if config['run_eval']:
        eval_classifiers(X_train, Y_train, X_test, Y_test)
        exit()

    if not config['classifier_type']:
        print('Need to specify model class.')
        print('Currently supported:')
        print('MultinomialNB, BernoulliNB, GaussianNB, LogisticRegression')
        exit()

    classifier = classifiers[config['classifier_type']]
    classifier.fit(X_train.todense(), Y_train)
    preds = classifier.predict(X_test.todense())
    print(classification_report(Y_test, preds))

    model = {}
    model['feature_log_prob'] = classifier.feature_log_prob_.tolist()
    model['class_log_prior'] = classifier.class_log_prior_.tolist()

    model_filename = config['output_filename'] + '.json'
    with open(model_filename, 'w') as f:
        json.dump(model, f)
    print('Dumped model to file ' + model_filename)
    exit()
