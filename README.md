Random Forest Classification Model

#Overview
This is a complete workflow for multi-class classification using Random Forest, including data preprocessing, model training, hyperparameter tuning, feature selection, model evaluation, and result output.

#Features
-Multi-class Random Forest modeling
-Hyperparameter tuning (mtry and ntree)
-Feature importance analysis
-Recursive Feature Elimination (via rfcv)
-Model performance evaluation (confusion matrix, multi-class metrics)
-Result export and model saving

#Dependencies
The following R packages are required:
-randomForest
-tidyverse
-caret
-pROC
-ggplot2
-reshape2
-scales
-ggprism
-ggpubr
-splines
-skimr
-DataExplorer

#Usage
#1. Data Preparation
Two CSV files are expected: one for training data and one for test data.
If you don't have pre-split datasets, you can use the automatic train/test split in Step 1.
Ensure your data includes a 'class' column as the target variable.
The script assumes the first two columns are metadata (will be converted to factors).

#2. Execution Workflow
1. Interactively select the training data CSV file
2. Interactively select the test data CSV file
3. The script automatically executes all analysis steps

#3. Sample Data and Results

This project provides ready-to-use example datasets ( trainset_RF and testset_RF), along with the corresponding analysis output file (output_RFresults) for reference.


##Step-by-Step Description

-Steps 1–2: Data Loading
Load training and test datasets
Convert the first two columns to factors
Check class distribution

-Step 3: Feature Preparation
Exclude the first two columns from features
Use the second column as the target variable

-Step 4: Hyperparameter Tuning (mtry)
Optimize mtry using repeated 5-fold cross-validation
Test mtry values ranging from 2 to 30

-Step 5: ntree Optimization
Evaluate performance across different ntree values (100–2000)
Visualize model performance vs. number of trees

-Step 6: Final Model Training
Train the optimized Random Forest model
Compute variable importance

-Step 7: Feature Importance Analysis
Generate importance plots
Rank features by accuracy-based importance
Save importance scores

-Steps 8–9: Model Evaluation
Evaluate on both training and test sets
Compute ROC curves and confusion matrices
Calculate multi-class summary metrics (precision, recall, F1, etc.)

-Step 10: Recursive Feature Elimination & New Model Construction
Use rfcv to identify the optimal number of features
Select top N most important features based on cross-validation results (can be manually adjusted)
Rebuild the dataset using only these top features
Retune hyperparameters using the same approach as in Steps 4–5
Perform classification predictions on the test set using this refined model

-Step 11: Model Saving
Save the trained model (user must specify file path)

-Step 12: Prediction Output Based on Optimal Feature Set
Generate a comprehensive results file containing:
List of selected top-N features
Confusion matrix
Multi-class summary metrics
Prediction results

-Step 13: External Data Prediction (Optional)
Predict classes for new external data
Ensure all selected features are present in the external dataset


###Important Notes###

1. Column Indexing: The script assumes the second column is the class label, and the first two columns are metadata. Adjust according to your actual data structure.

2. Factor Levels: Factor levels in the test data must exist in the training data; otherwise, an error will occur.

3. Feature Consistency: External prediction data must contain all selected feature columns.

4. Interactive File Selection: Uses file.choose() for data loading—manual file selection is required.

5. Hardcoded Parameters: Some parameters (e.g., top 34 features) are hardcoded. Update them based on your analysis results.

###Output Files###
importance_genus.txt: Feature importance scores for all features
importance_genus_top34.txt: Importance scores for the top 34 features
output_RFresults.txt: Comprehensive analysis results

###Troubleshooting###
"object not found" error: Ensure prior steps have been executed and data is loaded.
Prediction failure: Verify that external data contains all required feature columns.
Factor level mismatch: Confirm that test data factor levels are a subset of training data levels.
Data format issues: Ensure input files are standard CSV format with consistent column names and types.
