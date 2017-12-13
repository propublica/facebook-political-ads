"""
Extract entities
"""
import json

from bs4 import BeautifulSoup
import click
import spacy
import en_core_web_sm
from classifier.utilities import DB, entities_confs

# from LAWhttps://spacy.io/usage/linguistic-features#section-named-entities
LABELS = {
    'PERSON': 'Person',
    'NORP': 'Group',
    'ORG': 'Organization',
    'GPE': 'Region',
    'LOC': 'Location',
    'EVENT': 'Event',
    'FAC': 'Facility',
    'LAW': 'Law'
}

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
            nlp = en_core_web_sm.load()
            ads = DB.query("select * from ads where political_probability > 0.70 and lang = '%s' and entities = '[]'::jsonb" % lang)
            query = "update ads set entities=:entities where id=:id"
            updates = []

            for advert in ads:
                doc = BeautifulSoup(advert["html"], "html.parser")
                text = ' '.join([graf.get_text() for graf in doc.select("p")])

                update = {"id": advert["id"], "entities": set()}
                for ent in nlp(text).ents:
                    if ent.text in conf["exclude"] or ent.text.isspace():
                        continue

                    has_parent = False
                    for parent, children in conf["parents"].items():
                        if ent.text in children["entities"]:
                            has_parent = True
                            update["entities"].add((parent, children["label"]))

                    if not has_parent:
                        update["entities"].add((ent.text, ent.label_))

                update["entities"] = json.dumps([{"entity": e[0],
                                                  "entity_type": LABELS[e[1]]}
                                                 for e in update["entities"]
                                                 if e[1] in LABELS.keys()])
                updates.append(update)

                if len(updates) >= 100:
                    DB.bulk_query(query, updates)
                    updates = []

            if updates:
                DB.bulk_query(query, updates)
