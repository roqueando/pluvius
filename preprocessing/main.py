import pandas as pd
import numpy as np
from sklearn.preprocessing import normalize
from sklearn.model_selection import train_test_split

FEATURE_COLUMNS = [
        #"Data",
        #"Hora UTC",
    "PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)",
    "PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)",
    "PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)",
    "RADIACAO GLOBAL (KJ/m²)",
    "TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)",
    "TEMPERATURA DO PONTO DE ORVALHO (°C)",
    "TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)",
    "TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C)",
    "TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C)",
    "TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C)",
    "UMIDADE REL. MAX. NA HORA ANT. (AUT) (%)",
    "UMIDADE REL. MIN. NA HORA ANT. (AUT) (%)",
    "UMIDADE RELATIVA DO AR, HORARIA (%)",
    "VENTO, DIREÇÃO HORARIA (gr) (° (gr))",
    "VENTO, RAJADA MAXIMA (m/s)",
    "VENTO, VELOCIDADE HORARIA (m/s)",
]

LABEL_COL = 'PRECIPITAÇÃO TOTAL, HORÁRIO (mm)'

def format_date(df):
    df['Data'] = pd.to_datetime(df['Data'], format='%Y/%m/%d')
    df['Hora UTC'] = df['Hora UTC'].apply(lambda x: f"{int(x.split(" ")[0]):04d}")
    df['Hora UTC'] = pd.to_datetime(df['Hora UTC'], format='%H%M').dt.strftime('%H:%M:%S')
    return df

def clean(df):
    datetime_cols = ['Data', 'Hora UTC']
    fill_cols = [col for col in df.columns if col not in datetime_cols]
    df[fill_cols] = df[fill_cols].fillna(df[fill_cols].mean())
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


def main():
    df = pd.read_csv("../data/raw/2019.csv", low_memory=False, thousands=",")
    labels = df[LABEL_COL]

    print("renaming columns...")
    df = df[FEATURE_COLUMNS]

    #print("formatting date...")
    #df = format_date(df)

    print("casting...")
    df = cast(df)

    print("cleaning...")
    df = clean(df)

    print("normalizing...")
    df = norm(df)

    print("saving data...")
    X_train, X_test, y_train, y_test = train_test_split(df, labels, test_size=0.2, random_state=42)
    X_train.to_csv("../data/processed/2019_inputs_train.csv")
    y_train.to_csv("../data/processed/2019_labels_train.csv")

    X_test.to_csv("../data/processed/2019_inputs_test.csv")
    y_test.to_csv("../data/processed/2019_labels_test.csv")

if __name__ == "__main__":
    main()
