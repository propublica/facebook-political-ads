"""
Builds our classifiers
"""
import click
import dill
from classifier.utilities import (get_classifier, confs, get_vectorizer,
                                  classifier_path, train_classifier)

@click.option("--lang", help="Limit to language")


@click.command("build")
@click.pass_context
def build(ctx, lang):
    """
    Build classifiers for each of our languages.
    """
    for (directory, conf) in confs(ctx.obj["base"]):
        if lang and conf["language"] != lang:
            continue 
        model = train_classifier(get_classifier(), get_vectorizer(conf),
                                 directory, conf["language"])
        model_path = classifier_path(directory)
        with open(model_path, 'wb') as classy:
            dill.dump(model, classy)
        print("Saved model {}".format(model_path))
