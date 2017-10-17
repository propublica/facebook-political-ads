"""
Extract entities
"""
from collections import Counter

from bs4 import BeautifulSoup
import click
import spacy
from classifier.utilities import DB

LABELS = ['PER', 'NORP', 'ORG', 'GPE', 'LOC', 'PERSON', 'PRODUCT', 'EVENT', 'LAW', 'FAC']

@click.command("entities")
def entities():
    """
    Extract likely entitites from the database outputs a csv for now
    """
    nlp = spacy.load("en")
    ads = DB.query("select * from ads where political_probability > 0.80 and lang = 'en-US'")
    counter = Counter()
    for advert in ads:
        doc = BeautifulSoup(advert["html"], "html.parser")
        text = ' '.join([graf.get_text() for graf in doc.select("p")])

        for ent in nlp(text).ents:
            if ent.label_ in LABELS:
                counter[(ent.text, ent.label_)] += 1

    print("tag,type,count")
    for thing in counter.most_common(500):
        print("{},{},{}".format(thing[0][0], thing[0][1], thing[1]))
