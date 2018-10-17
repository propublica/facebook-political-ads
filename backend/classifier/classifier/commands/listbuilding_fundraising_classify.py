"""
Classify loops through all the ads and save the scores to the database.
"""
import click
import dill
from classifier.utilities import (classifier_path, get_vectorizer,
                                  confs, DB, get_text)

import spacy

# DATABASE_URL="postgres:///facebook_ads" pipenv run ./classify listbuilding_fundraising_classify --every

@click.command("listbuilding_fundraising_classify")
@click.option("--newest/--every",
              default=True,
              help="Classify all of the records")
@click.pass_context
def listbuilding_fundraising_classify(ctx, newest):
    """
    Classify the ads in the database at $DATABASE_URL.
    """
    lang = "en-US" # hard coded
    try:
        nlp = spacy.load('en_fbpac3label') # hard coded
    except OSError as e: 
        print("you need to do `pipenv install ~/code/fbpac-prodigy/packagedmodels/en_fbpac3label-2.0.0.tar.gz`")
        raise e

    if newest:
        print("Running newest")
        query = "select * from ads where political_probability > 0.70 and listbuilding_fundraising_proba is null"
        if lang:
            query = query + " and lang = '{}'".format(lang)
    else:
        print("Running every")
        query = "select * from ads where political_probability > 0.70"
        if lang:
            query = query + " and lang = '{}'".format(lang)

    total = "select count(*) as length from ({}) as t1;"
    length = DB.query(total.format(query))[0]["length"]
    records = DB.query(query)
    print("found {} ads".format(length))
    updates = []
    query = "update ads set listbuilding_fundraising_proba=:listbuilding_fundraising_proba where id=:id"
    idx = 0
    for record in records:
        idx += 1
        record_lang = "en-US" if record["lang"] == "en-IE" else record["lang"]
        
        text = get_text(record).replace("Learn More Watch Again Resume Video Learn More", '')
        doc = nlp(text)
        listbuilding_fundraising_proba = doc.cats["LISTBUILDING"] + doc.cats["FUNDRAISING"]

        update = {
            "id": record["id"],
            "listbuilding_fundraising_proba": listbuilding_fundraising_proba
        }
        updates.append(update)
        out = "Classified {pid[id]} ({info[idx]} of {info[length]}) with {pid[listbuilding_fundraising_proba]}"
        print(out.format(pid=update, info={"length": length, "idx": idx}))

        if len(updates) >= 100:
            DB.bulk_query(query, updates)
            updates = []

    if updates:
        DB.bulk_query(query, updates)
