from sklearn.naive_bayes import MultinomialNB, BernoulliNB, GaussianNB
from sklearn.linear_model import LogisticRegression
from sklearn.feature_extraction.text import HashingVectorizer
import json
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_curve, roc_auc_score
import matplotlib.pyplot as plt

with open("./political.json") as f:
    political = json.load(f)
with open("./not_political.json") as f:
    not_political = json.load(f)

pol = [(item, 1) for item in political]
npol = [(item, 0) for item in not_political]

data = pol + npol

train, test = train_test_split(data, test_size=0.5)

X_train, Y_train = zip(*train)
X_test, Y_test = zip(*test)
vectorizer = HashingVectorizer(alternate_sign=False, n_features=1000)
X_train = vectorizer.transform(X_train)
X_test = vectorizer.transform(X_test)

classifiers = [
    (GaussianNB(), "Gaussian"),
    (MultinomialNB(), "Multinomial"),
    (BernoulliNB(), "Bernoulli"),
    (LogisticRegression(), "Logistic")
]

for classifier, name in classifiers:
    classifier.fit(X_train.todense(), Y_train)
    preds = classifier.predict_proba(X_test.todense())[:, 1]
    fpr, tpr, _ = roc_curve(Y_test, preds)
    print("%s: %s" % (name, roc_auc_score(Y_test, preds)))
    plt.plot(fpr, tpr, label=name)

plt.xlabel("FPR")
plt.ylabel("TPR")
plt.legend()
plt.show()
