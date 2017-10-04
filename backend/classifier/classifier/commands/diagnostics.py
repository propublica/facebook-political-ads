"""
Test out all supported classifiers
"""
import click
from classifier.utilities import confs, get_classifiers, get_vectorizer, train_classifier


@click.command("diagnostics")
@click.pass_context
def diagnostics(ctx):
    """
    Warning! Slow! Run all classifiers against our database
    """
    for (directory, conf) in confs(ctx.obj["base"]):
        for name, classifier in get_classifiers().items():
            print("Report for {} in {}".format(name, conf["language"]))
            train_classifier(classifier, get_vectorizer(conf), directory, conf["language"])
