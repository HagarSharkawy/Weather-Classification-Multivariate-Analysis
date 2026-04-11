# 📊 Multivariate Analysis Project: Outliers & Discriminant Analysis

## 📌 Introduction

This project focuses on applying multivariate statistical techniques to analyze a dataset containing 150 observations and multiple variables, including at least one categorical class variable (with a minimum of 50 observations per class).

The main objectives are:

Detecting outliers using robust methods
Evaluating multivariate assumptions
Applying and comparing classification models
Assessing the impact of outliers on model performance

#### 1. BACON Algorithm (Full Dataset)

The BACON (Blocked Adaptive Computationally Efficient Outlier Nominators) method is applied to detect multivariate outliers.

Outputs:
Scatter plots with highlighted outliers
Distance (index) plots
Numerical summary (number of outliers, threshold)


#### 2. BACON by Class

BACON is applied separately for each class.

Purpose:
Detect within-group outliers
Compare outlier patterns across classes

## 📈 Discriminant Analysis

#### 3. Fisher Linear Discriminant Analysis (FLDA)
✔ Internal Validation
Classification performance within the dataset
✔ External Validatio
Model performance on test data

Accuracy and classification behavior
Strengths and limitations


#### 4. FLDA (p = 2, k = 2)

A simplified case is visualized with:

2 variables
2 classes
A plot is generated showing decision boundaries.

Separation between classes
Clarity of decision boundary

#### 5. Multinomial Logistic Regression
✔ Internal Validation
✔ External Validation

Performance VS FLDA
Flexibility VS assumptions

#### 6. Model Comparison

Internal validation (FLDA vs Multinomial)
External validation (FLDA vs Multinomial)


#### 7. Analysis Without Outliers

Repeat all classification steps after removing outliers.

Model performance
Stability
Changes in classification boundaries
