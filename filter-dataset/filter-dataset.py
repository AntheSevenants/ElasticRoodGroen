import warnings
warnings.filterwarnings("ignore", "\nPyarrow", DeprecationWarning)

import pandas as pd

from naive_dt_fix_py.naive_dt_fix import fix_participle_dt

print("[Filter] Loading dataset")
df = pd.read_csv("../RoodGroenAnthe.csv")

df = fix_participle_dt(df, "participle")