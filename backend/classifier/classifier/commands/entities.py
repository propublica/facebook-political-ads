"""
Extract entities
"""
from collections import Counter

from bs4 import BeautifulSoup
import click
import spacy
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
            nlp = spacy.load("en")
            ads = DB.query("select * from ads where political_probability > 0.80 and lang = '%s'" % lang)
            counter = Counter()
            for advert in ads:
                doc = BeautifulSoup(advert["html"], "html.parser")
                text = ' '.join([graf.get_text() for graf in doc.select("p")])

                for ent in nlp(text).ents:

                        counter[(ent.text, ent.label_)] += 1

            summarized = {}
            for parent, children in conf["parents"].items():
                children_entities = [thing for thing in counter.most_common(MOST_COMMON_COUNT) if thing[0][0] in children]
                summarized[parent] = children_entities
                
            print("parent_tag,child_tag,type,count")
            for parent, children_entities in summarized.items():
                for child in children_entities:
                    print("{},{},{},{}".format(parent, child[0][0], child[0][1], child[1]))

            # SM: Leaving here for now in case you want to revert
            # for thing in counter.most_common(500):
            #     print("{},{},{}".format(thing[0][0], thing[0][1], thing[1]))
