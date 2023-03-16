import pandas as pd

from transformers import AutoTokenizer, AutoModelForSequenceClassification
from tqdm.auto import tqdm

def divide_chunks(l, n):
    # looping till length l
    for i in range(0, len(l), n):
        yield l[i:i + n]

tokenizer = AutoTokenizer.from_pretrained("DTAI-KULeuven/robbert-v2-dutch-sentiment")
model = AutoModelForSequenceClassification.from_pretrained("DTAI-KULeuven/robbert-v2-dutch-sentiment")

df = pd.read_csv("../RoodGroenAnthe.csv")

chunks = divide_chunks(list(df["sentence"]), 100)
#print(f"Split {len(df['sentence'])} sentences into {len(chunks)} chunks")

for chunk in tqdm(chunks):
    inputs = tokenizer(chunk, return_tensors='pt', padding=True)
    output = model(**inputs)
    # Convert to regular floats
    output = output.logits.softmax(dim=-1).detach().tolist()