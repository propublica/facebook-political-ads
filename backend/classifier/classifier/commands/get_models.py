"""
Downloads existing classifiers
"""
import click
import boto3
import botocore
from classifier.utilities import ( confs )

@click.option("--lang", help="Limit to language")
@click.command("get_models")
@click.pass_context
def get_models(ctx, lang):
    """
    download classifiers for each of our languages.
    """
    for (directory, conf) in confs(ctx.obj["base"]):
        if lang and conf["language"] != lang:
            continue 
        model_path = "fbpac-models/{}/classifier.dill".format(conf["language"])
        s3 = boto3.resource('s3')

        print(model_path)
        try:
            s3.Bucket("pp-data").download_file(model_path, "data/{}/classifier.dill".format(conf["language"]))
        except botocore.exceptions.ClientError as e:
            if e.response['Error']['Code'] == "404":
                print("The object does not exist.")
            else:
                raise
