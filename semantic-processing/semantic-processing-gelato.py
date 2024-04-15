from tqdm.auto import tqdm
from lxml import etree
import pandas as pd

semcor = pd.read_csv("../output/RoodGroenAnthe_coefficients_semantics.csv")

cornetto = etree.parse("../data/gelato.le.xml")

print("Cornetto loaded")

def get_semantic_properties(semantic_type):
    control = False
    attributive = False
    spatial = False
    cognitive = False # TODO change to 'cognitive'
    dynamic = False

    if semantic_type.startswith("action"):
        control = True
        dynamic = True
    elif semantic_type.startswith("echprod"):
        control = True
        dynamic = True
        attributive = True
    elif semantic_type.startswith("mvmt"):
        control = True
        dynamic = True
        spatial = True
    elif semantic_type.startswith("cognt"):
        control = True
        dynamic = True
        cognitive = True
    elif semantic_type.startswith("state"):
        # states do not have any positive properties
        pass
    elif semantic_type.startswith("possess"):
        attributive = True
    elif semantic_type.startswith("location3"):
        control = True
        spatial = True
    elif semantic_type.startswith("location"):
        spatial = True
    elif semantic_type.startswith("stcognt"):
        cognitive = True
    elif semantic_type.startswith("process"):
        dynamic = True
    elif semantic_type.startswith("prmvmt"):
        dynamic = True
        spatial = True
    elif semantic_type.startswith("procognt"):
        dynamic = True
        cognitive = True

    return control, attributive, spatial, cognitive, dynamic

def get_cornetto_info(sense):
    sense_node = cornetto.xpath(f"//Sense[@senseId='{sense}']")
    if len(sense_node) == 0:
        return None
    sense_node = sense_node[0]

    semantic_type = None
    semantic_feature_set = None
    semantic_type_node = sense_node.xpath("./Semantics-verb/semanticTypes")
    if len(semantic_type_node) != 0:
        semantic_type = semantic_type_node[0].get("semanticType")

        # Semantic feature set available
        if len(semantic_type_node) == 2:
            semantic_feature_set = semantic_type_node[1].get("semanticFeatureSet")

    control, attributive, spatial, cognitive, dynamic = get_semantic_properties(semantic_feature_set)

    polarity = None
    polarity_node = sense_node.xpath("./Sentiment")
    if len(polarity_node) != 0:
        polarity = polarity_node[0].get("polarity")

    valency = None
    transitivity = None
    lexical_entry = sense_node.getparent()
    syntactic_behaviour_node = lexical_entry.xpath("./SyntacticBehaviour")
    if len(syntactic_behaviour_node) != 0:
        valency = syntactic_behaviour_node[0].get("valency")
        transitivity = syntactic_behaviour_node[0].get("transitivity")

    return {"sense": sense,
            "semantic_type": semantic_type,
            "semantic_feature_set": semantic_feature_set,
            "polarity": polarity,
            "valency": valency,
            "transitivity": transitivity,
            "control": control, 
            "attributive": attributive, 
            "spatial": spatial, 
            "cognitive": cognitive, 
            "dynamic": dynamic}


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
