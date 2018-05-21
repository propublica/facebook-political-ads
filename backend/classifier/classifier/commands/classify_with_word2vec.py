"""
Classify loops through all the ads and save the scores to the database.
"""
import click
import dill
from classifier.utilities import (classifier_path, get_vectorizer,
                                  confs, DB, get_text)
from gensim import Doc2Vec
from sklearn.ensemble import ExtraTreesClassifier


@click.command("classifyw2v")
@click.option("--newest/--every",
              default=True,
              help="Classify all of the records")
@click.option("--lang", help="Limit to language")
@click.pass_context
def classifyw2v(ctx, newest, lang):
    """
    Classify the ads in the database at $DATABASE_URL.
    """

    model = load_model("/Users/jm-admin/code/congress2/lib/word2vec/models/model_doc2vec_communications_5_100_0.025_just_member_id_trigrams_dbow.model")




    classifiers = dict()
    for (directory, conf) in confs(ctx.obj["base"]):
        with open(classifier_path(directory), 'rb') as classy:
            classifiers[conf["language"]] = {
                "classifier": dill.load(classy),
                "vectorizer": lambda texts: model.infer_vector(tokenize_words(texts[0]))
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



from tokenizer import TreebankWordTokenizer
treebank_word_tokenizer = TreebankWordTokenizer()
alpha_re = re.compile("^[a-zA-Z]+'?[a-zA-Z]*?$") # allow single apostrophes but not double apostrophes: note, this doesn't allow 'ere
def clean(text):
    text = text.replace("\xa0", " ") # turns NBSPs into regular spaces (was a problem in Del Kilili's statements)
                                     # but we do want to preserve line beraks
    # text = text.encode("ascii","ignore").decode("utf-8") # this is bad, nukes characters from Luján, etc.'s names
    text = text.replace("“", "\"").replace("”", "\"")
    text = re.sub(r"([a-z]+)\.([A-Z][a-z]+)", "\\1. \\2", text)
    text = text.replace("\\r\\n", "\r\n" )
    text = re.sub(r"# ?# ?#", "", text)

    text = re.sub(r"\t[A-Za-z]+( [A-Za-z]+)?\n\t", "", text) # removes one or two words set off by tabs, since they're probably site chrome
    return text
def tokenize_words(text):
    return [s.lower() for s in treebank_word_tokenizer.tokenize(clean(text)) if re.match(alpha_re, s)]


def load_model(args_dot_model):
  if args_dot_model:
    model_name = args_dot_model
  else:
    if not exists(most_recent_model_filename_filename):
      print("Oops! You need to train a model before you can use this function (or specify the location of your model with the --model flag).")
      print("In most cases, running `python train_doc2vec_model.py inputs/your/input/folder` should do the trick!")
      exit(1)
    with open(most_recent_model_filename_filename, "r") as f:
        model_name =  (f.read().strip())
    if not exists(model_name):
      print("Oops! Your model may have been deleted or moved. Either specify its location with the --model flag or fix the path in .texttoolkit_model_filename (or train a new model).")
      exit(1)
  return Doc2Vec.load(model_name)

