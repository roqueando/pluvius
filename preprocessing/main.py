import pandas as pd
import numpy as np
from sklearn.preprocessing import normalize, MinMaxScaler, StandardScaler
from sklearn.decomposition import PCA
from sklearn.model_selection import train_test_split

COLUMNS = [
    "date",
    "hour",
    "rain",
    "pmax",
    "pmin",
    "tmax",
    "tmin",
    "dpmax",
    "dpmin",
    "hmax",
    "hmin"
]

LABEL_COL = 'rain'


def date_ohe(df):
    dates = df['date'].transform(lambda x: x.split('/'))
    print(dates)


def format_date(df):
    df['Data'] = pd.to_datetime(df['Data'], format='%Y/%m/%d')
    df['Hora UTC'] = df['Hora UTC'].apply(
        lambda x: f"{int(x.split(" ")[0]):04d}")
    df['Hora UTC'] = pd.to_datetime(
        df['Hora UTC'], format='%H%M').dt.strftime('%H:%M:%S')
    return df


def clean(df):
    datetime_cols = ['Data', 'Hora UTC']
    fill_cols = [col for col in df.columns if col not in datetime_cols]
    df[fill_cols] = df[fill_cols].fillna(df[fill_cols].mean())
    return df


def cast_labels(df):
    df = df.replace(',', '.', regex=True)
    df = df.transform(lambda x: x.astype(np.float32))
    return df


def cast(df):
    datetime_cols = ['Data', 'Hora UTC']
    fill_cols = [col for col in df.columns if col not in datetime_cols]
    df[fill_cols] = df[fill_cols].replace(',', '.', regex=True)
    df[fill_cols] = df[fill_cols].transform(lambda x: x.astype(np.float32))
    return df


def norm(df):
    datetime_cols = ['Data', 'Hora UTC']
    fill_cols = [col for col in df.columns if col not in datetime_cols]
    x = df[fill_cols].values
    x = normalize(x)
    return pd.DataFrame(x)


def scale(df):
    datetime_cols = ['Data', 'Hora UTC']
    fill_cols = [col for col in df.columns if col not in datetime_cols]
    x = df[fill_cols].values
    scaler = MinMaxScaler()
    scaler.fit(x)
    x = scaler.transform(x)
    return pd.DataFrame(x)


def reduction(df):
    datetime_cols = ['Data', 'Hora UTC']
    fill_cols = [col for col in df.columns if col not in datetime_cols]
    x = df[fill_cols].values

    pca = PCA(n_components=3)
    x = pca.fit_transform(x)
    return pd.DataFrame(x)


def main():
    df = pd.read_csv("../data/raw/2019.csv", low_memory=False, thousands=",")
    date_ohe(df)


if __name__ == "__main__":
    main()
