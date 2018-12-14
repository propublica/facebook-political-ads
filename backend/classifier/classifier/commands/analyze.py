"""
Analyze permutes an ad's text to find which words contribute most to its political rating.
"""
import click
import dill
import re
from classifier.utilities import (classifier_path, get_vectorizer,
                                  confs, DB, get_text)


def flatten(l): 
    return [a for b in l for a in b]

def permute_text(text):
    words = ' '.join(text.split()).split()
    bigrams = list(zip(words[:-1], words[1:]))
    return [(word, ' '.join(words[:i] + words[i+1:]) ) for i, word in enumerate(words)] + [(bigram, ' '.join(flatten(bigrams[:i-1] + bigrams[i+2:]) )) for i, bigram in enumerate(bigrams)]

def probability_difference(classifier, permuted_text, baseline):
    vectorized_text = classifier["vectorizer"].transform([permuted_text])
    probability = classifier["classifier"].predict_proba(vectorized_text)[0][1]
    # print("{}: {}".format(baseline - probability, deleted_word))
    return baseline - probability


def clean_text(text, advertiser):
    clean_text = text.replace(advertiser, "ADVERTISER") if advertiser else text
    clean_text = re.sub("Paid for by[^·]+", '', clean_text)
    clean_text = re.sub(r'https?\://www\.facebook\.com/[^ ]+', '', clean_text)
    clean_text = re.sub(r'https?\://(www\.)?actblue\.com/[^ ]+', 'actblue', clean_text)
    return clean_text

@click.command("analyze")
@click.option("--id", help="Which ID to analyze")
@click.pass_context
def analyze(ctx, id):
    """
    Analyze permutes an ad's text to find which words contribute most to its political rating.
    """
    classifiers = dict()
    for (directory, conf) in confs(ctx.obj["base"]):
        with open(classifier_path(directory), 'rb') as classy:
            classifiers[conf["language"]] = {
                "classifier": dill.load(classy),
                "vectorizer": get_vectorizer(conf)
            }
    records = DB.query("""select * from ads where id = '{}'""".format(id))

    idx = 0
    for record in records:
        record_lang = record["lang"]
        if record_lang in classifiers:
            classifier = classifiers[record_lang]
            text = clean_text(get_text(record), record["advertiser"])
            permuted_texts = permute_text(text)
            vectorized_baseline_text = classifier["vectorizer"].transform([text])
            baseline = classifier["classifier"].predict_proba(vectorized_baseline_text)[0][1]

            diffs = [(deleted_word, probability_difference(classifier, permuted_text, baseline)) for (deleted_word, permuted_text)  in permuted_texts]

            print("text: {}".format(text))
            print("original probability: {}".format(baseline))
            biggest_diffs = sorted(diffs, key=lambda word_diff: -abs(word_diff[1]))[:4]
            print("top difference-makers:")
            for (deleted_word, permuted_text) in biggest_diffs:
                print(" - {}, {}".format(deleted_word, permuted_text))

            # for (deleted_word, permuted_text) in permuted_texts:
            #     vectorized_text = classifier["vectorizer"].transform([permuted_text])
            #     probability = classifier["classifier"].predict_proba(vectorized_text)[0][1]
            #     print("{}: {}".format(baseline - probability, deleted_word))

