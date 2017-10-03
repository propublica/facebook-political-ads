"""
Builds our classifiers
"""
import os
import json
import click
import dill
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
from sqlalchemy.sql import text
from classifier.utilities import (get_classifier, equalize_classes, get_text,
                                  confs, DB, get_vectorizer, classifier_path)

def load_ads_from_psql(lang):
    """
    Grab ads that users have rated for our classifier
    """
    records = DB.query("""
      select
        html,
        political::float / (political::float + not_political::float) as score
      from ads
        where lang = '{}'
        and (political + not_political) > 0;
     """.format(text(lang)))

    data = []
    for record in records:
        score = round(record["score"])
        data.append((get_text(record["html"]), score))
    return data


@click.command("build")
@click.pass_context
def build(ctx):
    """
    Build classifiers for each of our languages.
    """
    for (directory, conf) in confs(ctx.obj["base"]):
        with open(os.path.join(directory, "seeds.json"), 'rb') as json_posts:
            posts = json.load(json_posts)
            data = [(item, 1.0) for item in posts['political']]
            data.extend([(item, 0.0) for item in posts['not_political']])
            print("seed length: %s" % len(data))
            data.extend(load_ads_from_psql(conf["language"]))
            print("num unique samples: %s" % len(data))
            train, test = train_test_split(data)
            x_train, y_train = zip(*train)
            x_test, y_test = zip(*test)
            vectorizer = get_vectorizer(conf)
            x_train = vectorizer.transform(x_train)
            x_test = vectorizer.transform(x_test)
            x_train, y_train = equalize_classes(x_train, y_train)
            print("final size of training data: %s" % x_train.shape[0])
            classifier = get_classifier()
            classifier.fit(x_train.todense(), y_train)
            print(classification_report(y_test, classifier.predict(x_test.todense())))
            model_path = classifier_path(directory)
            with open(model_path, 'wb') as classy:
                dill.dump(classifier, classy)
            print("Saved model {}".format(model_path))
