import pandas as pd

def main():
    df = pd.read_csv("../data/processed/2019_inputs_train.csv")
    print(df.std())

if __name__ == '__main__':
    main()
