"""
Builds our classifiers
"""
import os
import json
import click
import dill
from classifier.utilities import (get_classifier, confs, get_vectorizer,
                                  classifier_path, train_classifier)

@click.command("build")
@click.pass_context
def build(ctx):
    """
    Build classifiers for each of our languages.
    """
    for (directory, conf) in confs(ctx.obj["base"]):
        model = train_classifier(get_classifier(), get_vectorizer(conf),
                                 directory, conf["language"])[0]
        model_path = classifier_path(directory)
        with open(model_path, 'wb') as classy:
            dill.dump(model, classy)
        print("Saved model {}".format(model_path))
