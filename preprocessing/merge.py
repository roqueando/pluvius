import pandas as pd
import os
import sys

def main():
    year = sys.argv[1]
    year_folder = os.listdir(f'./{year}')
    new_list = list(map(lambda x: f'./{year}/{x}', year_folder))

    df = pd.concat([pd.read_csv(f, on_bad_lines='skip', delimiter=";", thousands=",", skiprows=8, encoding="latin1") for f in new_list], ignore_index=True)
    df.to_csv(f"./{year}.csv")

if __name__ == "__main__":
    main()
