"""
Classify loops through all the ads and save the scores to the database.
"""
import click
import dill
from classifier.utilities import (classifier_path, get_vectorizer,
                                  confs, DB, get_text)


@click.command("classify")
@click.option("--newest/--every",
              default=True,
              help="Classify all of the records")
@click.option("--lang", help="Limit to language")
@click.pass_context
def classify(ctx, newest, lang):
    """
    Classify the ads in the database at $DATABASE_URL.
    """
    classifiers = dict()
    for (directory, conf) in confs(ctx.obj["base"]):
        with open(classifier_path(directory), 'rb') as classy:
            classifiers[conf["language"]] = {
                "classifier": dill.load(classy),
                "vectorizer": get_vectorizer(conf)
            }

    if newest:
        print("Running newest")
        query = "select * from ads where political_probability = 0"
        if lang:
            query = query + " and lang = '{}'".format(lang)
        else:
            langs = map(lambda x: "'{}'".format(x), classifiers.keys())
            langs = ','.join(langs)

            query = query + " and lang in ({})".format(langs)
    else:
        print("Running every")
        query = "select * from ads"
        if lang:
            query = query + " where lang = '{}'".format(lang)

    total = "select count(*) as length from ({}) as t1;"
    length = DB.query(total.format(query))[0]["length"]
    records = DB.query(query)
    print("found {} ads".format(length))
    updates = []
    query = "update ads set political_probability=:probability where id=:id"
    idx = 0
    for record in records:
        idx += 1
        record_lang = "en-US" if record["lang"] == "en-IE" else record["lang"]
        if record_lang in classifiers:
            classifier = classifiers[record_lang]
            text = classifier["vectorizer"].transform([get_text(record)])
            probability = classifier["classifier"].predict_proba(text)[0][1]
            update = {
                "id": record["id"],
                "probability": probability
            }
            if record["political_probability"] > update["probability"] and record["political_probability"] >= 0.70 and update["probability"] < 0.70 and not record["suppressed"]:
                print("refusing to downgrade probability of ad {}".format(record["id"]))
            updates.append(update)
            out = "Classified {pid[id]} ({info[idx]} of {info[length]}) with {pid[probability]}"
            print(out.format(pid=update, info={"length": length, "idx": idx}))

            if len(updates) >= 100:
                DB.bulk_query(query, updates)
                updates = []

    if updates:
        DB.bulk_query(query, updates)
