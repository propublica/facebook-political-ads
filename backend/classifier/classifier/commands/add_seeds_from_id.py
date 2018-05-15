"""
Grab additional seeds from the database.

We realized that the Canadian classifier, in particular, wasn't working very well. So we picked out some ads from the database by ID and are adding them to the seeds file with this command.
"""
import json
import os
import click
import facebook
import requests
from classifier.utilities import confs, DB, get_text
# DATABASE_URL="postgres:///facebook_ads" pipenv run ./classify add_seeds_from_id --language en-CA



@click.command("add_seeds_from_id")
@click.pass_context
@click.argument("language")
def add_seeds_from_id(ctx, language):
    """
    Create a list of seed posts for our classifier by language
    """
    options = None
    for directory, conf in confs(ctx.obj["base"]):
        if conf["language"] == language:
            options = conf
            conf_dir = directory
            break

    if options is None:
        print("Couldn't find a config for {}".format(language))
        exit()

    with open(os.path.join(conf_dir, 'additional_seed_ids.json'), 'r') as additional_seeds_file:
        additional_seed_ids = json.load(additional_seeds_file)

    with open(os.path.join(conf_dir, 'seeds.json'), 'r') as old_seeds_file:
        seeds = json.load(old_seeds_file)

    for politicality in ["political", "not_political"]:
        records = DB.query("select * from ads where id in ('{}')".format("','".join(map(str, additional_seed_ids[politicality]))))
        for record in records:
            seeds[politicality].append(get_text(record))

    for politicality in ["political", "not_political"]:
        seeds[politicality] = list(set(seeds[politicality]))

    with open(os.path.join(conf_dir, 'seeds.json'), 'w') as out:
        json.dump(seeds, out)




