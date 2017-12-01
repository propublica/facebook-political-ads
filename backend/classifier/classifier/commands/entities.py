"""
Extract entities
"""
from collections import Counter

from bs4 import BeautifulSoup
import click
import spacy
import json

from classifier.utilities import DB, entities_confs

LABELS = ['PER', 'NORP', 'ORG', 'GPE', 'LOC', 'PERSON', 'PRODUCT', 'EVENT', 'LAW', 'FAC']
MOST_COMMON_COUNT = 500
@click.command("entities")
@click.pass_context
def entities(ctx):
    """
    Extract likely entitites from the database outputs a csv for now
    """
    for (directory, conf) in entities_confs(ctx.obj["base"]):
        if conf:
            lang = directory.split('/')[1]

            print("running entity extraction for %s" % lang)
            nlp = spacy.load("en")
            ads = DB.query("select * from ads where political_probability > 0.80 and lang = '%s'" % lang)
            counter = Counter()
            query = "update ads set entities=:entities where id=:id"
            updates = []

            for advert in ads:
                doc = BeautifulSoup(advert["html"], "html.parser")
                text = ' '.join([graf.get_text() for graf in doc.select("p")])

                update = {"id": advert["id"], "entities": set()}
                for ent in nlp(text).ents:
                    
                    if ent.text in conf["exclude"]:
                        continue

                    has_parent = False
                    for parent, children in conf["parents"].items():
                        if ent.text in children["entities"]:
                            has_parent = True   
                            update["entities"].add((parent, children["label"]))
                    
                    if not has_parent:
                        update["entities"].add((ent.text, ent.label_))

                update["entities"] = json.dumps([{"name":e[0], "label": e[1]} for e in update["entities"]])
                updates.append(update)

                if len(updates) >= 100:
                    DB.bulk_query(query, updates)
                    updates = []
            
            if updates:
                DB.bulk_query(query, updates)
