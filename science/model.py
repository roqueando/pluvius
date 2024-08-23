import matplotlib.pyplot as plt
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV
from skl2onnx import to_onnx

FEATURE_COLUMNS = [
    'Temp. Ins. (C)', 'Temp. Max. (C)', 'Temp. Min. (C)',
    'Umi. Ins. (%)',	'Umi. Max. (%)',	'Umi. Min. (%)',
    'Pto Orvalho Ins. (C)',	'Pto Orvalho Max. (C)',	'Pto Orvalho Min. (C)',
    'Pressao Ins. (hPa)',	'Pressao Max. (hPa)',	'Pressao Min. (hPa)',
    'Vel. Vento (m/s)',	'Dir. Vento (m/s)',	'Raj. Vento (m/s)',
    'Radiacao (KJ/m²)',
]
LABEL_COL = 'Chuva (mm)'

def section(text):
    print("-"*100)
    print()
    print(text)

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

section("starting data cleaning")
df = pd.read_csv("../data/weather.csv", on_bad_lines='skip', delimiter=';', thousands=',')

section("renaming columns...")
cols = [
    'Data',
    'Hora (UTC)',
    'Temp. Ins. (C)', 'Temp. Max. (C)', 'Temp. Min. (C)',
    'Umi. Ins. (%)',	'Umi. Max. (%)',	'Umi. Min. (%)',
    'Pto Orvalho Ins. (C)',	'Pto Orvalho Max. (C)',	'Pto Orvalho Min. (C)',
    'Pressao Ins. (hPa)',	'Pressao Max. (hPa)',	'Pressao Min. (hPa)',
    'Vel. Vento (m/s)',	'Dir. Vento (m/s)',	'Raj. Vento (m/s)',
    'Radiacao (KJ/m²)',
    'Chuva (mm)'
]

df = df[cols]

section(df.head())

section("formatting date...")
df = format_date(df)

section(df.head())
section("cleaning nullables...")

df = clean(df)
section("dataframe cleaned!")

section("splitting data...")
X = df[FEATURE_COLUMNS]
y = df[LABEL_COL]
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
section(f"X_train length: {len(X_train)}, X_test len: {len(X_test)}")

#section("searching for better params with grid search...")
param_grid = {'n_estimators': [50, 100, 150, 200], 'max_depth': [5, 10, 15, 20]}
#grid_search = GridSearchCV(estimator=RandomForestRegressor(), param_grid=param_grid, cv=5)
#grid_search.fit(X_train, y_train)
#section(f"best parameters: {grid_search.best_params_}")

section("training...")
rf = RandomForestRegressor(n_estimators=50, max_depth=15)
rf.fit(X_train, y_train)
t = X_train[:1].values[0]
onx = to_onnx(rf, t)
with open("rf_pluvius.onnx", "wb") as f:
    f.write(onx.SerializeToString())

section("testing...")
pred = rf.predict(X_test)

real_df = pd.read_csv("../data/22_08_2024.csv", on_bad_lines='skip', delimiter=';', thousands=',')
real_df = format_date(real_df)

section("trying with real data...")
real_df = clean(real_df)

X_real = real_df[FEATURE_COLUMNS]
real_pred = rf.predict(X_real)
real_df['Previsao Chuva (mm)'] = real_pred
print(real_df)
