"""Util python code for download INMET dataset."""
import urllib.request
import zipfile
import os
import pandas as pd
import shutil


def fix_types(df, col):
    df[col] = df[col].map(lambda x: x if isinstance(
        x, float) or isinstance(x, int) else float(x.replace(',', '.')))


def merge(year):
    """Merge all files in year folder into a single CSV."""
    year_folder = os.listdir(f'data/raw/{year}')
    new_list = list(map(lambda x: f'data/raw/{year}/{x}', year_folder))
    df = pd.concat(
        [pd.read_csv(f,
                     on_bad_lines='skip',
                     delimiter=";",
                     thousands=",",
                     skiprows=8,
                     encoding="latin1") for f in new_list]
    )
    df = df[[
        'Data',
        'Hora UTC',
        'PRECIPITAÇÃO TOTAL, HORÁRIO (mm)',
        'PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)',
        'PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)',
        'TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)',
        'TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C)',
        'TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C)',
        'TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C)',
        'UMIDADE REL. MAX. NA HORA ANT. (AUT) (%)',
        'UMIDADE REL. MIN. NA HORA ANT. (AUT) (%)'
    ]]
    df = df.rename(columns={
        'Data': "date",
        'Hora UTC': "hour",
        'PRECIPITAÇÃO TOTAL, HORÁRIO (mm)': "rain",
        'PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)': "pmax",
        'PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)': "pmin",
        'TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)': "tmax",
        'TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C)': "tmin",
        'TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C)': "dpmax",
        'TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C)': "dpmin",
        'UMIDADE REL. MAX. NA HORA ANT. (AUT) (%)': "hmax",
        'UMIDADE REL. MIN. NA HORA ANT. (AUT) (%)': "hmin"
    })

    fix_types(df, 'rain')
    fix_types(df, 'tmax')
    fix_types(df, 'tmin')
    fix_types(df, 'dpmax')
    fix_types(df, 'dpmin')

    df.to_csv(f"data/raw/{year}.csv")


def clean_dir(year):
    """Delete the zipped file and the directory."""
    os.remove(f"data/raw/{year}.zip")
    shutil.rmtree(f"data/raw/{year}")


def main():
    """Download INMET dataset and merge all files in a single CSV."""
    # TODO: set this as dynamic years
    filename = "data/raw/2019.zip"
    print("downloading INMET dataset from 2019...")
    urllib.request.urlretrieve(
        "https://portal.inmet.gov.br/uploads/dadoshistoricos/2019.zip",
        filename
    )

    print("extracting dataset...")
    with zipfile.ZipFile(filename, 'r') as zip_ref:
        zip_ref.extractall("data/raw")

    print("merging dataset")
    merge(2019)

    print("cleaning...")
    clean_dir(2019)


if __name__ == "__main__":
    main()
