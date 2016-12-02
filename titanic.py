# -*- coding: utf-8 -*-
"""
Created on Fri Sep 30 11:01:34 2016

@author: Jonny
"""

import sys
import os
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import train_test_split
from sklearn.metrics import accuracy_score

os.chdir(r"C:\Users\Jonny\titanic")
data = pd.read_csv("transformed_train.csv")


data = data[['Survived', 'PassengerId','Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked', 
                 'Title', 'FamilySize', 'FamilyID', 'NumOfCabin', 'TicketNum']]

fields_to_transform =['Sex','Embarked', 'Title', 'FamilyID', 'TicketNum']

data_transformed = pd.get_dummies(data[fields_to_transform], dummy_na = True)

non_cat_fields = list(set(list(data.columns.values)) - set(fields_to_transform))
numerical_fields = data[non_cat_fields]
data_transformed = pd.concat([data_transformed, numerical_fields], axis = 1)

train, test = train_test_split(data_transformed, test_size = 0.2)

features_train = train.drop('Survived', 1)
features_test = test.drop('Survived', 1)

target_train = train['Survived']
target_test = test['Survived']

clf = RandomForestClassifier(n_estimators=100,oob_score=True,criterion='entropy')
y, _ = pd.factorize(target_train)
clf.fit(features_train, y)

#print out number of rows used and out of bag score:
print("OOB SCORE:", clf.oob_score_)

prediction = clf.predict(features_test)
pd.crosstab(test['Survived'], prediction, rownames = ['actual'], colnames = ['preds'])

accuracy_score(test['Survived'], prediction)
