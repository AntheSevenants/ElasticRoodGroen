import pandas as pd

df = pd.read_csv("../data/RoodGroenAnthePolarity.csv")
df = df[["sentence_id", 'negative', 'positive']]

df.to_csv("../data/RoodGroenAnthePolarity.csv", index=False)