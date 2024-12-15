import pandas as pd
import numpy as np
from sklearn.preprocessing import normalize, MinMaxScaler, StandardScaler, OneHotEncoder
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

def norm(df):
    datetime_cols = ['Data', 'Hora UTC']
    fill_cols = [col for col in df.columns if col not in datetime_cols]
    x = df[fill_cols].values
    x = normalize(x)
    return pd.DataFrame(x)

def one_hot_date(x):
    return [int(xi) for xi in x.split("/")]

def one_hot_hour(x):
    splitted = x.split(" ")[0]
    hour = int(splitted[:2])
    minute = int(splitted[2:])

    return (hour, minute)

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

def fill_nullables(df, cols):
    for col in cols:
        df[col].fillna(value=df[col].mean(), inplace=True)


def main():
    df = pd.read_sql_table("2019_enriched", 'postgresql://pluvius:local_password@localhost:5432/raw')

    encoded_date = df['date'].transform(one_hot_date).values
    encoded_hours = df['hour'].transform(one_hot_hour)

    new_df = {
        'year': [ed[0] for ed in encoded_date],
        'month': [ed[1] for ed in encoded_date],
        'day': [ed[2] for ed in encoded_date],
        'hour': [eh[0] for eh in encoded_hours],
        'minute': [eh[1] for eh in encoded_hours],
        'pmax': df['pmax'],
        'pmin': df['pmin'],
        'tmax': df['tmax'],
        'tmin': df['tmin'],
        'dpmax': df['dpmax'],
        'dpmin': df['dpmin'],
        'hmax': df['hmax'],
        'hmin': df['hmin'],
        'pdiff': df['pdiff'],
        'tdiff': df['tdiff'],
        'dpdiff': df['dpdiff'],
        'hdiff': df['hdiff']
    }
    new_df = pd.DataFrame(data=new_df)

    fill_nullables(new_df, [
        'pmax',
        'pmin',
        'tmax',
        'tmin',
        'dpmax',
        'dpmin',
        'hmax',
        'hmin',
        'pdiff',
        'tdiff',
        'dpdiff',
        'hdiff'
    ])
    print(new_df.isnull().sum())


if __name__ == "__main__":
    main()
