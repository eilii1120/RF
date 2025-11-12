# RF
# Install required packages (uncomment if running for the first time)
# install.packages("")
# install.packages("tidyverse")
# install.packages("skimr")
# install.packages("DataExplorer")
# install.packages("caret")
# install.packages("randomForest")
# install.packages("reshape2")
# install.packages("scales")
# install.packages("ggprism")
# install.packages("ggpubr")

# library()
library(randomForest)
library(tidyverse)
library(skimr)
library(DataExplorer)
library(caret)
library(pROC)
library(ggplot2)
library(splines)
library(reshape2)
library(scales)
library(ggprism)
library(ggpubr)

# === Step 1: Initial data loading
# boston <- read.csv(file.choose()) # Interactive file selection 
# boston <- as.data.frame(boston)
# boston <- na.pass(boston)
# boston <- na.omit(boston)
# na.action = na.omit 
# na.action = na.pass 

# skim(boston) # Quick data summary

# Visualize missing values # Visualize missing values
# boston[is.na(boston)] <- 0  # Replace NAs with 0 – may introduce bias! Use cautiously

# for (i in c(1:5)) { # Convert first 5 columns to factors (e.g., sample ID, metadata)
#   boston[,i] <- factor(boston[,i])
# }
# 
# dim(boston)

# table(boston$class)
# set.seed(1)
# trains <- createDataPartition(
#   y = boston$class,
#   p = 0.7,
#   list = F
# )
# traindata <- boston[trains,]
# testdata <- boston[-trains,]


# === Step 1: Initial data loading
# === Step 2: Load training and test datasets
# Two interactive file dialogs will appear: select TRAIN then TEST CSV files
traindata <- read.csv(file.choose()) # Choose training data file
traindata <- as.data.frame(traindata)
# Adjust column indices based on your actual data structure
for (i in c(1:2)) { # Convert first 5 columns to factors (e.g., sample ID, metadata)
  traindata[,i] <- factor(traindata[,i])
}
testdata <- read.csv(file.choose()) # Choose test data file
testdata <- as.data.frame(testdata)
for (i in c(1:2)) {
  testdata[,i] <- factor(testdata[,i])
}

# Check class distribution in train and test sets
table(traindata$class)
table(testdata$class)

# === Step 3: Prepare features and labels for modeling
# Assumes:
# - Column 1: ID or metadata (to drop)
# - Column 2: Class label
# - Columns 5+: Features (e.g., microbial abundances)
# Verify this matches your data! If 'class' is the last column, use: traindata[, -ncol(traindata)]
X_train <- traindata[, -(1:2)] # Features: exclude first 2 columns
y_train <- as.factor(traindata[, 2]) # Labels: column 2

# Initial Random Forest model (500 trees)
model <- randomForest(x = X_train, y = y_train, ntree = 500)

# === Step 4: Hyperparameter tuning for mtry using caret
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
grid1 <- expand.grid(mtry = c(2:30))  # Test mtry from 2 to 30
rf_model <- train(x = X_train, y = y_train, 
                  method = "rf",  
                  trControl = ctrl,
                  tuneGrid = grid1,)
print(rf_model)

# === Step 5: Evaluate impact of ntree (with fixed mtry)
grid2 <- expand.grid(mtry = c(23))  # – should ideally use best mtry from rf_model$bestTune

modellist <- list()
for (ntree in seq(100, 2000, by=100)) { 
  set.seed(101)
  fit <- train(x = X_train, 
               y = y_train, 
               method="rf",
               metric="Accuracy", 
               tuneGrid=grid2,
               trControl=ctrl, 
               ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
results <- resamples(modellist)
summary(results)

# Visualize performance across ntree values
bwplot(results)  
dotplot(results)  
densityplot(results)  

# === Step 6: Final model with tuned parameters
set.seed(1001)
fit_rf_clsm <- randomForest(x = X_train,
                            y = y_train,
                            mtry = 23, # Should be updated based on rf_model$bestTune$mtry
                            ntree = 1400, # Should be chosen based on results (e.g., 1400 if better)
                            importance = T)
print(fit_rf_clsm)

# Plot error vs. number of trees
plot(fit_rf_clsm,main = "ERROR & TREES")
legend("top",
       legend = colnames(fit_rf_clsm$err.rate),
       lty = 1:6,
       col = 1:6,
       horiz = T,
       cex = 0.9)

# Plot margin distribution
plot(randomForest::margin(fit_rf_clsm), main = '')

# === Step 7: Variable importance analysis
varImpPlot(fit_rf_clsm,main ="varImpPlot")
varImpPlot(fit_rf_clsm,main = "varImpPlot",type = 1)
varImpPlot(fit_rf_clsm,main = "varImpPlot",type = 2)

# Extract and sort importance scores
importance_genus <- data.frame(importance(fit_rf_clsm))
# importance_genus <- importance_genus[order(importance_genus$MeanDecreaseGini,
#                                            decreasing=TRUE),]
importance_genus <- importance_genus[order(importance_genus$MeanDecreaseAccuracy,
                                           decreasing=TRUE),]
head(importance_genus)

# Save importance scores to file
write.table(importance_genus,"importance_genus.txt",
            sep = '\t',col.names = NA,quote = FALSE) 

# === Step 8: Evaluate on training set
trainpredprob <- predict(fit_rf_clsm,
                         newdata = traindata,
                         type = "prob")

multiclass.roc(response = traindata$class,
               predictor = trainpredprob)

trainpredlab <- predict(fit_rf_clsm,
                        newdata = traindata,
                        type = "class")

confusionMatrix_train <- confusionMatrix(data = trainpredlab,
                reference = traindata$class,
                mode = "everything")

multiClassSummary(
  data.frame(obs = traindata$class, pred=trainpredlab),
  lev = levels(traindata$class)
)


# === Step 9: Predict on original test set ===
# X_test <- testdata[, -(1:2)] 
# y_test <- as.factor(testdata[, 2]) 
testpredprob <- predict(fit_rf_clsm,
                        newdata = testdata,
                        type = "prob")
multiclass.roc(response = testdata$class,
               predictor = testpredprob)

testpredlab <- predict(fit_rf_clsm,
                       newdata = testdata,
                       type = "class")
confusionMatrix_test <- confusionMatrix(data = testpredlab,
                   reference = testdata$class,
                   mode = "everything")
confusionMatrix_test

multiClassSummary(
  data.frame(obs = testdata$class,pred=testpredlab),
  lev = levels(testdata$class)
)

# === Step 10: Recursive feature elimination (rfcv) for optimal feature subset
set.seed(10001)
otu_train.cv <- replicate(5, rfcv(traindata[,-(1:5)],  # Feature matrix
                                  traindata$class, # Labels
                                  cv.fold = 10,
                                  step = 1.5), simplify = FALSE)
# Process rfcv results
otu_train.cv <- data.frame(sapply(otu_train.cv, '[[', 'error.cv'))
otu_train.cv$otus <- rownames(otu_train.cv)
otu_train.cv <- reshape2::melt(otu_train.cv, id = 'otus')
otu_train.cv$otus <- as.numeric(as.character(otu_train.cv$otus))
otu_train.cv.mean <- aggregate(otu_train.cv$value, 
                               by = list(otu_train.cv$otus),
                               FUN = mean)
head(otu_train.cv.mean, 18)

# Plot CV error vs. number of features
p <- ggplot(otu_train.cv,aes(otus,value)) +
     geom_smooth(se = FALSE, method = 'glm',formula = y~ns(x,6)) +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(color = 'black',fill = 'transparent')) +
     labs(title = '',x='Number of genus',y='Cross-validation error')
p
p + geom_vline(xintercept = 34) # Manually choose optimal feature count (e.g., 34)


# Select top N most important features
importance_genus[1:34,] # Preview top 34
importance_Ngenus <- importance_genus[1:34,] 
write.table(importance_genus[1:34, ], 
            'importance_genus_top34.txt',
            sep = '\t',
            col.names = NA, 
            quote = FALSE)

varImpPlot(fit_rf_clsm, 
           n.var = min(34, nrow(fit_rf_clsm$importance)), 
           main = 'Top 34 - variable importance',
           type = 1)

# Subset data to top N features + class
genus_select <- rownames(importance_genus)[1:34]
genus_train_top <- traindata[ ,c(genus_select, 'class')]
genus_test_top <- testdata[ ,c(genus_select, 'class')]

# Prepare new training data (dynamic column indexing)
x_train1 <- genus_train_top[, -ncol(genus_train_top)] # All except last column
y_train1 <- as.factor(genus_train_top[, ncol(genus_train_top)]) # Last column = class

# Train new RF model on top 34 features
fit_rf_clsm_test1 <- randomForest(x = x_train1,
                                 y = y_train1,
                                ntree = 500,  
                                importance = TRUE  
                                 )
fit_rf_clsm_test1


# Tune mtry again (smaller range for fewer features)
ctrl1 <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
grid3 <- expand.grid(mtry = c(2:15)) 
fit_rf_clsm_test2 <- train(x = x_train1, y = y_train1, 
                  method = "rf",   
                  trControl = ctrl1,
                  tuneGrid = grid3,)
print(fit_rf_clsm_test2)

# Test different ntree values with fixed mtry
grid4 <- expand.grid(mtry = c(3)) # Should reflect best mtry from fit_rf_clsm_test2

modellist1 <- list()
for (ntree in seq(100, 2000, by=100)) {  
  set.seed(100003)
  fit1 <- train(x = x_train1, 
                y = y_train1, 
                method="rf",
               metric="Accuracy", 
               tuneGrid=grid4,
               trControl=ctrl1, 
               ntree=ntree)
  key1 <- toString(ntree)
  modellist1[[key1]] <- fit1
}
results1 <- resamples(modellist1)
summary(results1)

# Visualize results
bwplot(results1)
dotplot(results1)  
densityplot(results1)  

# Final model on top N features
set.seed(250209)
fit_rf_clsm1 <- randomForest(x = x_train1,
                            y = y_train1,
                            mtry = 3, # Update based on tuning (e.g., 3 if that was best)
                            ntree = 800, # Choose based on results1 (e.g., where accuracy plateaus)
                            importance = T)

print(fit_rf_clsm1)

# "ERROR & TREES"
plot(fit_rf_clsm1,main = "ERROR & TREES")

plot(randomForest::margin(fit_rf_clsm1), main = '')

# Evaluate on reduced training set
trainpredprob <- predict(fit_rf_clsm1,
                         newdata = genus_train_top,
                         type = "prob")

multiclass.roc(response = genus_train_top$class,
               predictor = trainpredprob)

trainpredlab <- predict(fit_rf_clsm1,
                        newdata = genus_train_top,
                        type = "class")

confusionMatrix(data = trainpredlab,
                reference = genus_train_top$class,
                mode = "everything")

# Evaluate on reduced test set
testpredprob <- predict(fit_rf_clsm1,
                        newdata = genus_test_top,
                        type = "prob")

multiclass.roc(response = genus_test_top$class,
               predictor = testpredprob)

testpredlab <- predict(fit_rf_clsm1,
                       newdata = genus_test_top,
                       type = "class")

confusion_matrix <- confusionMatrix(data = testpredlab,
                reference = genus_test_top$class,
                mode = "everything")
confusion_matrix
multiClassSummary(
  data.frame(obs = genus_test_top$class,pred=testpredlab),
  lev = levels(genus_test_top$class)
)

results <- data.frame(Actual = genus_test_top$class, Predicted = testpredlab)

confusion_matrix_df <- as.data.frame.matrix(confusion_matrix$table)
confusion_matrix_df

# === Step 11: Save and load model
# Specify a valid file path!
saveRDS(fit_rf_clsm, 
 file = "") # replace with actual path

# To load later:
# fit_rf_clsm1 <- readRDS("your_model_path.rds")

# loading
fit_rf_clsm1 <- readRDS("")


# === Step 12: Save comprehensive output results ===
# 1. Top selected features (from genus_select)
top_features_df <- data.frame(Feature = genus_select, Rank = 1:length(genus_select))

# 2. Confusion matrix from external test set (testdata1)
confusion_matrix_df <- as.data.frame.matrix(confusion_matrix$table)

# 3. Multi-class summary metrics
multi_summary <- multiClassSummary(
  data.frame(obs = genus_test_top$class, pred = testpredlab),
  lev = levels(genus_test_top$class)
)

# 4. Prediction results (actual vs predicted)
prediction_results <- data.frame(Actual = genus_test_top$class, Predicted = testpredlab)

# Convert multi_summary to a named vector to preserve metric names
multi_summary_named <- as.data.frame(t(as.matrix(multi_summary)))

# Combine all components into a single text string with section headers
output_text <- paste(
  "=== Top Selected Features ===",
  paste(top_features_df, collapse = "\n"),
  "\n\n=== Confusion Matrix ===",
  paste(confusion_matrix_df, collapse = "\n"),
  "\n\n=== Multi-Class Summary ===",
  paste(c(colnames(multi_summary_named), unlist(multi_summary_named)), collapse = "\n"),
  "\n\n=== Prediction Results ===",
  paste(prediction_results, collapse = "\n"),
  sep = "\n"
)

# Write the combined text to a single file
writeLines(output_text, "output_RFresults.txt")





# === Step 13: Predict on new external data (if have a new external data)
# testdata123 <- read.csv(file.choose())
# testdata123 <- as.data.frame(testdata123)
# for (i in c(1:2)) { 
#   testdata123[,i] <- factor(testdata123[,i])
# }
# 
# 
# # CRITICAL: testdata1 must contain ALL columns in 'genus_select' 
# # Missing columns will cause prediction to fail!
# testpredprob <- predict(fit_rf_clsm1, 
#                         newdata = testdata123, 
#                         type = "prob")
# 
# multiclass.roc(response = testdata123$class, 
#                predictor = testpredprob)
# 
# testpredlab <- predict(fit_rf_clsm1, 
#                        newdata = testdata123,
#                        type = "class")
# 
# confusion_matrix <- confusionMatrix(data = testpredlab,
#                                     reference = testdata123$class,
#                                     mode = "everything")
# 
# multiClassSummary(
#   data.frame(obs = testdata123$class,pred=testpredlab),
#   lev = levels(testdata123$class)
# )
# 
# results <- data.frame(Actual = testdata123$class, Predicted = testpredlab)

