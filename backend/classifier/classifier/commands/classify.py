"""
Classify loops through all the ads and save the scores to the database.
"""
import click
import dill
from classifier.utilities import classifier_path, get_vectorizer, confs, DB, get_text

@click.command("classify")
@click.option("--newest/--every",
              default=True,
              help="Classify all of the records")
@click.pass_context
def classify(ctx, newest):
    """
    Classify the ads in the database at $DATABASE_URL.
    """
    if newest:
        print("Running newest")
        query = "select * from ads where political_probability = 0"
    else:
        print("Running every")
        query = "select * from ads"

    length = DB.query("select count(*) as length from ({}) as t1;".format(query))[0]["length"]
    records = DB.query(query)
    classifiers = dict()
    for (directory, conf) in confs(ctx.obj["base"]):
        with open(classifier_path(directory), 'rb') as classy:
            classifiers[conf["language"]] = {
                "classifier": dill.load(classy),
                "vectorizer": get_vectorizer(conf)
            }

    print("found {} ads".format(length))
    updates = []
    query = "update ads set political_probability=:probability where id=:id"
    idx = 0
    for record in records:
        idx += 1
        if record["lang"] in classifiers:
            classifier = classifiers[record["lang"]]
            text = classifier["vectorizer"].transform([get_text(record["html"])])
            update = {
                "id": record["id"],
                "probability": classifier["classifier"].predict_proba(text)[0][1]
            }
            updates.append(update)

            print("Classified {p[id]} ({l[idx]} of {l[length]}) with {p[probability]}"
                  .format(p=update, l={"length":length, "idx":idx}))

            if len(updates) >= 100:
                DB.bulk_query(query, updates)
                updates = []

    if updates:
        DB.bulk_query(query, updates)
