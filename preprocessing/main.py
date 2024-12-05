import pandas as pd
import numpy as np
from sklearn.preprocessing import normalize, MinMaxScaler, StandardScaler
from sklearn.decomposition import PCA
from sklearn.model_selection import train_test_split

FEATURE_COLUMNS = [
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

FEATURE_COLUMNS_V2 = [
    #"PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)",
    "PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)",
    "PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)",
    #"RADIACAO GLOBAL (KJ/m²)",
    #"TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)",
    #"TEMPERATURA DO PONTO DE ORVALHO (°C)",
    "TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)",
    "TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C)",
    "TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C)",
    "TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C)",
    "UMIDADE REL. MAX. NA HORA ANT. (AUT) (%)",
    "UMIDADE REL. MIN. NA HORA ANT. (AUT) (%)",
    #"UMIDADE RELATIVA DO AR, HORARIA (%)",
    #"VENTO, DIREÇÃO HORARIA (gr) (° (gr))",
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
    labels = df[LABEL_COL]

    print("renaming columns...")
    df = df[FEATURE_COLUMNS_V2]

    print("casting...")
    df = cast(df)

    print("cleaning...")
    df = clean(df)

    ldf = labels.fillna(0.0)
    ldf = cast_labels(ldf)

    print("normalizing...")
    df = norm(df)

    print("reduction...")
    df = reduction(df)

    print("saving data...")
    x_train, x_test, y_train, y_test = train_test_split(df, ldf,
                                                        test_size=0.2,
                                                        random_state=42)
    x_train.to_csv("../data/processed/2019_inputs_train.csv")
    y_train.to_csv("../data/processed/2019_labels_train.csv")

    print(f"X_train: {x_train.shape}, y_train: {y_train.shape}")

    x_test.to_csv("../data/processed/2019_inputs_test.csv")
    y_test.to_csv("../data/processed/2019_labels_test.csv")


if __name__ == "__main__":
    main()
