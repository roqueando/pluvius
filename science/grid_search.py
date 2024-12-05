import pandas as pd
import json

from sklearn.model_selection import GridSearchCV, train_test_split
from sklearn.linear_model import LinearRegression

import numpy as np

print("importing cleaned data")
X = pd.read_csv("../data/processed/2019_inputs_train.csv", on_bad_lines='skip')
y = pd.read_csv("../data/processed/2019_labels_train.csv", on_bad_lines='skip')

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
print(f"X_train length: {len(X_train)}, X_test len: {len(X_test)}")

print("searching for better params with grid search...")
param_grid = {'n_estimators': [10, 25, 50, 100, 150, 200], 'max_depth': [2, 5, 10, 15, 20, 30, 45]}
grid_search = GridSearchCV(estimator=LinearRegression(), param_grid=param_grid, cv=5)
grid_search.fit(X_train, y_train)

print(f"dumping best parameters: {grid_search.best_params_}")
json_object = json.dumps(grid_search.best_params_, indent=2)

with open('../data/processed/params.json', 'w') as outfile:
    outfile.write(json_object)
