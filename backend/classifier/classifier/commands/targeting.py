"""
Extract targeting parameters
"""
from collections import Counter
from bs4 import BeautifulSoup
import click
from classifier.utilities import DB


@click.command("targeting")
def targeting():
    """
    Extract the targeting parameters we've seen
    """
    ads = DB.query("""
       select * from ads
       where political_probability > 0.70 and targeting is not null
    """)
    counter = Counter()
    for advert in ads:
        doc = BeautifulSoup(advert["targeting"], "html.parser")
        targets = [bold for bold in doc.select("b")
                   if bold.get('id') != "ad_prefs_advertiser"]
        counter.update(targets)

    print("parameter,type,count")
    for (target, count) in counter.items():
        print("{},{},{}".format(target.get_text(), target.get('id'), count))
