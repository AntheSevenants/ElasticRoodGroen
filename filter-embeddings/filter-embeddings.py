import argparse
import warnings
warnings.filterwarnings("ignore", "\nPyarrow", DeprecationWarning)
import pandas as pd
from tqdm.auto import tqdm

parser = argparse.ArgumentParser(
    description='filter-embeddings - filter only the required embeddings from a larger embeddings file')
parser.add_argument('embeddings_path', type=str, help='Path to the embeddings')
parser.add_argument('coefficients_path', type=str, help='Path to the coefficients')
parser.add_argument('--output_filename', type=str, default="../output/embeddings.txt",
                    help='Filename for the output file')
args = parser.parse_args()

# Load the coefficients dataset to check for which words we have to get embeddigns
coefficients = pd.read_csv(args.coefficients_path)
features = coefficients["feature"].tolist()

# Holds the filtered embedding lines
output_text = ""

# This variable will be set to the vector size once it's read from the file
vector_size = -1

vector_count = -1
output_vector_count = 0

with open(args.embeddings_path, 'rt') as reader:
    first_line = reader.readline()

    # e.g. 3110718 320
    # vector count is the first number
    # vector size is the second number
    vector_count, vector_size = [int(a) for a in first_line.rstrip().split(" ")]

    # Should continue from the second line
    for line in tqdm(reader, total=vector_count, desc="Embeddings processed"):
        # e.g. de -0.4 0.02 0.02 0.04 ...
        components = line.rstrip().split(" ")
        # word index is the first item
        word = components[0]

        if word in features:
            # This keeps the newline at the end
            output_text += line
            output_vector_count += 1

# Turn into valid w2v type
output_text = f"{str(output_vector_count)} {vector_size}\n{output_text}"

# Output to file
with open(args.output_filename, "wt") as writer:
    writer.write(output_text)