from sklearn.ensemble import RandomForestRegressor
from sklearn.pipeline import Pipeline
import onnx
import onnxruntime as ort
import pandas as pd
import numpy as np
from pickle import load

FEATURE_COLUMNS = [
    'Temp. Ins. (C)', 'Temp. Max. (C)', 'Temp. Min. (C)',
    'Umi. Ins. (%)',	'Umi. Max. (%)',	'Umi. Min. (%)',
    'Pto Orvalho Ins. (C)',	'Pto Orvalho Max. (C)',	'Pto Orvalho Min. (C)',
    'Pressao Ins. (hPa)',	'Pressao Max. (hPa)',	'Pressao Min. (hPa)',
    'Vel. Vento (m/s)',	'Dir. Vento (m/s)',	'Raj. Vento (m/s)',
    'Radiacao (KJ/m²)',
]
LABEL_COL = 'Chuva (mm)'

def format_date(df):
    df['Data'] = pd.to_datetime(df['Data'], format='%d/%m/%Y')
    df['Hora (UTC)'] = df['Hora (UTC)'].apply(lambda x: f"{int(x):04d}")
    df['Hora (UTC)'] = pd.to_datetime(df['Hora (UTC)'], format='%H%M').dt.strftime('%H:%M:%S')
    return df

def clean(df):
    datetime_cols = ['Data', 'Hora (UTC)']
    fill_cols = [col for col in df.columns if col not in datetime_cols]
    df[fill_cols] = df[fill_cols].fillna(df[fill_cols].mean())
    return df

def cast(df):
    datetime_cols = ['Data', 'Hora (UTC)']
    fill_cols = [col for col in df.columns if col not in datetime_cols]
    df[fill_cols] = df[fill_cols].transform(lambda x: x.astype(float))
    return df

def run():
    df = pd.read_csv("../data/simple.csv", on_bad_lines='skip', delimiter=';', thousands=',')
    df = format_date(df)
    df = clean(df)
    df = cast(df)

    x_input = df[FEATURE_COLUMNS]
    with open("rf_pluvius.pkl", "rb") as f:
        model = load(f)

    return model.predict(x_input)
