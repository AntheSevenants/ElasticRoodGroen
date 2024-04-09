from glob import glob
from tqdm.auto import tqdm

import pandas as pd
import re

# First, load all semcor files which match the verb type
semcor_path = "../data/dutsemcor/*.v.xml"
semcor = pd.DataFrame({ "path": glob(semcor_path) })

# Then, extract the verb lemma from the filename
semcor["lemma"] = semcor["path"].str.extract(".*/(.*).v.xml")

# Read the elastic net output, with lemmas
df = pd.read_csv("../output/RoodGroenAnthe_coefficients_infused.csv")

# Only keep lemmas which have a DutchSemCor entry
semcor = semcor.merge(right=df, on="lemma")

# Yes, parsing XML with regex is bad
# But the XML files are very simple, and some DutchSemCor files are HUGE
# so: this is a memory safe solution. don't @ me
def get_senses_memory_safe(path):
    senses = []

    with open(path, "rt") as reader:
        for line in reader:
            if len(line) == 0:
                break

            sense_search = re.search('.*sense=\"(.*?)\".*', line, re.IGNORECASE)
            if not sense_search:
                continue

            senses.append(sense_search.group(1))

    return senses

# https://stackoverflow.com/a/1518632
def get_topmost_sense(lst):
    return max(set(lst), key=lst.count)

def get_topmost_sense_from_path(path):
    senses = get_senses_memory_safe(path)
    topmost_sense = get_topmost_sense(senses)

    return topmost_sense

tqdm.pandas(desc="Lemmas processed")

semcor["dominant_sense"] = semcor.progress_apply(lambda row: get_topmost_sense_from_path(row["path"]),
                                        axis=1)

semcor.to_csv("../output/RoodGroenAnthe_coefficients_semantics.csv", index=False)