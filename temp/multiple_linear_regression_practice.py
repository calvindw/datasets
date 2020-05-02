
# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import glob as glob

# Importing the dataset
df = pd.read_csv('https://raw.githubusercontent.com/calvindw/datasets/master/temp/advertising.csv')


#select all the columns up to last one
X = df.iloc[:,:-1].values
#select all the columns just the last one
y = df.iloc[:,3].values
#y = y.reshape(-1,1)


#split train and test data
from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(X,y, test_size=0.2, random_state = 0)



#from sklearn.preprocessing import StandardScaler
#create instance of the class
#sc = StandardScaler()
#this will normalise all the values in the same range
#X = sc.fit_transform(X)
#X_train[:, 0:] = sc.fit_transform(X_train[:, 0:])
#X_test[:, 0:] = sc.transform(X_test[:, 0:])

#fitting simple linear regression to the training set
from sklearn.linear_model import LinearRegression

#create regressor object
regressor = LinearRegression()

#choose method, choose which  model, choose fit. y_train is the target value
regressor.fit(X=X_train,y=y_train)


#predicting the test set results
#create a vector of predicted salaries / dependent variable
y_pred = regressor.predict(X_test)



plt.scatter(X_train[:,0], y_train, color = 'red')
plt.plot(X_train[:,0], regressor.predict(X_train), color = 'blue')
plt.title('TV advertising  vs sales (Training set)')
plt.xlabel('TV advertising')
plt.ylabel('sales')
plt.show()

#visualising  test set result
plt.scatter(X_test[:,0], y_test, color = 'red')
plt.plot(X_train[:,0], regressor.predict(X_train), color = 'blue')
plt.title('TV advertising  vs sales (test set)')
plt.xlabel('TV advertising')
plt.ylabel('sales')
plt.show()
