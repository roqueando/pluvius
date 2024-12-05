# %%
import pandas as pd
filename = "../data/processed/2019_inputs_train.csv"
df = pd.read_csv(filename)
df = df.dropna()
df.to_csv(filename)
