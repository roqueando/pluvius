import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from skl2onnx import to_onnx

print("importing cleaned data")
X = pd.read_csv("../data/processed/2019_inputs_train.csv", on_bad_lines='skip')
y = pd.read_csv("../data/processed/2019_labels_train.csv", on_bad_lines='skip')

# %% separating
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2,
                                                    random_state=42)

print(f"X_train length: {len(X_train)}, X_test len: {len(X_test)}")

print("training...")
lr = LinearRegression()
lr.fit(X_train, y_train)
t = X_train[:1].values[0]
onx = to_onnx(lr, t)

print("saving model...")
with open("../data/evaluation/pluvius_lr-v1.onnx", "wb") as f:
    f.write(onx.SerializeToString())

print(f"model score: {lr.score(X_test, y_test)}")
print("testing...")
pred = lr.predict(X_test)
print(f"predicted: {pred}")

#real_df = pd.read_csv("../data/22_08_2024.csv", on_bad_lines='skip', delimiter=';', thousands=',')
#real_df = format_date(real_df)

#print("trying with real data...")
#real_df = clean(real_df)
#
#X_real = real_df[FEATURE_COLUMNS]
#real_pred = rf.predict(X_real)
#real_df['Previsao Chuva (mm)'] = real_pred
#print(real_df)
