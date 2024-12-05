"""Util python code for download INMET dataset."""
import urllib.request
import zipfile
import os
import pandas as pd
import shutil


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
                     encoding="latin1") for f in new_list],
        ignore_index=True
        )
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
