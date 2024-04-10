from tqdm.auto import tqdm
from lxml import etree
import pandas as pd

semcor = pd.read_csv("../output/RoodGroenAnthe_coefficients_semantics.csv")

cornetto = etree.parse("../data/cdb2.0.lu.xml")

print("Cornetto loaded")


def get_cornetto_info(sense):
    entry = cornetto.xpath(f"//cdb_lu[@c_lu_id='{sense}']")[0]

    try:
        morpho_type = entry.xpath("./morphology_verb/morpho-type")[0].text.strip()
        conj_type = entry.xpath(
            "./morphology_verb/flex-conjugation/flex-conjugationtype")[0].text.strip()

        valency = entry.xpath("./syntax_verb/sy-valency")[0].text.strip()
        transitivity = entry.xpath("./syntax_verb/sy-trans")[0].text.strip()
        sclass = entry.xpath("./syntax_verb/sy-class")[0].text.strip()
        peraux = entry.xpath("./syntax_verb/sy-peraux")[0].text.strip()
        reflexive = entry.xpath("./syntax_verb/sy-reflexiv")[0].text.strip()
        subject = entry.xpath("./syntax_verb/sy-subject")[0].text.strip()

        sem_type = entry.xpath("./semantics_verb/sem-type")[0].text.strip()

        return {"sense": sense,
                "morpho_type": morpho_type,
                "conj_type": conj_type,
                "valency": valency,
                "transitivity": transitivity,
                "sclass": sclass,
                "peraux": peraux,
                "reflexive": reflexive,
                "subject": subject,
                "sem_type": sem_type}
    except:
        #print(sense)
        return None

 
infos = []
for sense in tqdm(semcor["dominant_sense"]):
    cornetto_info = get_cornetto_info(sense)
    if cornetto_info is not None:
        infos.append(cornetto_info)

# Create data frame from Cornetto info
df = pd.DataFrame.from_records(infos)
# Drop the duplicates (two lemmas can mean the same thing, that's how we get duplicates)
df = df.drop_duplicates(subset=["sense"], keep=False)

df.to_csv("../output/cornetto_info.csv", index=False)