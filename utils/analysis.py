# %%
import pandas as pd

filename = "../data/raw/2019.csv"

df = pd.read_csv(filename)
df.dtypes
# df = df['rain'].transform(lambda x: x.replace(',', '.'))
# df.dtypes
# df.astype({'rain': 'float64'}).dtypes
