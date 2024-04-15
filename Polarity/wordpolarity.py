import pandas as pd

from transformers import AutoTokenizer, AutoModelForSequenceClassification
from tqdm.auto import tqdm
from math import ceil

def divide_chunks(l, n):
    # looping till length l
    for i in range(0, len(l), n):
        yield l[i:i + n]

tokenizer = AutoTokenizer.from_pretrained("DTAI-KULeuven/robbert-v2-dutch-sentiment")
model = AutoModelForSequenceClassification.from_pretrained("DTAI-KULeuven/robbert-v2-dutch-sentiment")

df = pd.read_csv("../output/RoodGroenAnthe_coefficients_semantics.csv")

#df = df.head(1000)

CHUNK_SIZE = 100

chunks = divide_chunks(list(df["lemma"]), CHUNK_SIZE)
total_chunks = ceil(len(df["lemma"]) / CHUNK_SIZE)
#print(f"Split {len(df['sentence'])} sentences into {len(chunks)} chunks")

output_logits = []

for chunk in tqdm(chunks, total=total_chunks):
    inputs = tokenizer(chunk, return_tensors='pt', padding=True)
    output = model(**inputs)
    # Convert to regular floats
    output = output.logits.softmax(dim=-1).detach().tolist()

    output_logits += output

df["negative"] = list(map(lambda polarity: polarity[0], output_logits))
df["positive"] = list(map(lambda polarity: polarity[1], output_logits))

df.to_csv("../output/RoodGroenAntheWordPolarity.csv", index=False)