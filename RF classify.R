# RF

# install.packages("")
install.packages("tidyverse")
install.packages("skimr")
install.packages("DataExplorer")
install.packages("caret")
install.packages("randomForest")
install.packages("reshape2")
install.packages("scales")

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

# loading
boston <- read.csv(file.choose())
boston <- as.data.frame(boston)
# boston <- na.pass(boston)
# boston <- na.omit(boston)
# na.action = na.omit 
# na.action = na.pass 

skim(boston)

plot_missing(boston)
boston[is.na(boston)] <- 0 

for (i in c(1:2)) {
  boston[,i] <- factor(boston[,i])
}

dim(boston)

table(boston$class)
set.seed(1)
trains <- createDataPartition(
  y = boston$class,
  p = 0.7,
  list = F
)
traindata <- boston[trains,]
testdata <- boston[-trains,]

# test
# traindata <- read.csv(file.choose())
# traindata <- as.data.frame(traindata)
# for (i in c(1:5)) {
#   traindata[,i] <- factor(traindata[,i])
# }
# testdata <- read.csv(file.choose())
# testdata <- as.data.frame(testdata)
# for (i in c(1:5)) {
#   testdata[,i] <- factor(testdata[,i])
# }

table(traindata$class)
table(testdata$class)

X_train <- traindata[, -(1:2)] #traindata x
y_train <- as.factor(traindata[, 2]) #traindata y
model <- randomForest(x = X_train, y = y_train, ntree = 500)

ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
grid1 <- expand.grid(mtry = c(2:30))  
rf_model <- train(x = X_train, y = y_train, 
                  method = "rf",  
                  trControl = ctrl,
                  tuneGrid = grid1,)
print(rf_model)


grid2 <- expand.grid(mtry = c(30))

modellist <- list()
for (ntree in seq(100, 2000, by=100)) { 
  set.seed(101)
  fit <- train(x = X_train, y = y_train, method="rf",
               metric="Accuracy", tuneGrid=grid2,
               trControl=ctrl, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
results <- resamples(modellist)

summary(results)

bwplot(results)  
dotplot(results)  
densityplot(results)  


set.seed(1001)
fit_rf_clsm <- randomForest(x = X_train,
                            y = y_train,
                            mtry = 30,
                            ntree = 600,
                            importance = T)
print(fit_rf_clsm)

plot(fit_rf_clsm,main = "ERROR & TREES")
legend("top",
       legend = colnames(fit_rf_clsm$err.rate),
       lty = 1:6,
       col = 1:6,
       horiz = T,
       cex = 0.9)
plot(randomForest::margin(fit_rf_clsm), main = '')

# 变量重要性
varImpPlot(fit_rf_clsm,main ="varImpPlot")
varImpPlot(fit_rf_clsm,main = "varImpPlot",type = 1)
varImpPlot(fit_rf_clsm,main = "varImpPlot",type = 2)

importance_genus <- data.frame(importance(fit_rf_clsm))
importance_genus <- importance_genus[order(importance_genus$MeanDecreaseGini,
                                           decreasing=TRUE),]
importance_genus <- importance_genus[order(importance_genus$MeanDecreaseAccuracy,
                                           decreasing=TRUE),]
head(importance_genus)

write.table(importance_genus,"importance_genus.txt",
            sep = '\t',col.names = NA,quote = FALSE) 

# predict
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


varImpPlot(fit_rf_clsm, n.var = min(20, nrow(fit_rf_clsm$importance)),
           main = 'Top 20 - variable importance')

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


set.seed(10001)
otu_train.cv <- replicate(5, rfcv(traindata[,-(1:2)], 
                                  traindata$class,
                                  cv.fold = 10,
                                  step = 1.5), simplify = FALSE)
otu_train.cv <- data.frame(sapply(otu_train.cv, '[[', 'error.cv'))
otu_train.cv$otus <- rownames(otu_train.cv)
otu_train.cv <- reshape2::melt(otu_train.cv, id = 'otus')
otu_train.cv$otus <- as.numeric(as.character(otu_train.cv$otus))
otu_train.cv.mean <- aggregate(otu_train.cv$value, 
                               by = list(otu_train.cv$otus),
                               FUN = mean)
head(otu_train.cv.mean, 18)


p <- ggplot(otu_train.cv,aes(otus,value)) +
     geom_smooth(se = FALSE, method = 'glm',formula = y~ns(x,6)) +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(color = 'black',fill = 'transparent')) +
     labs(title = '',x='Number of genus',y='Cross-validation error')
p
p + geom_vline(xintercept = 70)

importance_genus[1:19,]
importance_Ngenus <- importance_genus[1:70,]
write.table(importance_genus[1:70, ],
            'importance_genus_top70.txt',
            sep = '\t',
            col.names = NA, 
            quote = FALSE)

varImpPlot(fit_rf_clsm,main ="varImpPlot")
varImpPlot(fit_rf_clsm,main = "varImpPlot",type = 1)
varImpPlot(fit_rf_clsm,main = "varImpPlot",type = 2)
varImpPlot(fit_rf_clsm, 
           n.var = min(19, nrow(fit_rf_clsm$importance)), 
           main = 'Top 19 - variable importance',
           type = 1)

genus_select <- rownames(importance_genus)[1:70]
genus_train_top <- traindata[ ,c(genus_select, 'class')]
genus_test_top <- testdata[ ,c(genus_select, 'class')]

x_train1 <- genus_train_top[, -70 - 1] 
y_train1 <- as.factor(genus_train_top[, 70 + 1])  

fit_rf_clsm_test1 <- randomForest(x = x_train1,
                                 y = y_train1,
                                ntree = 500,  
                                importance = TRUE  
                                 )
fit_rf_clsm_test1

ctrl1 <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
grid3 <- expand.grid(mtry = c(2:15)) 

fit_rf_clsm_test2 <- train(x = x_train1, y = y_train1, 
                  method = "rf",   
                  trControl = ctrl1,
                  tuneGrid = grid3,)
print(fit_rf_clsm_test2)


grid4 <- expand.grid(mtry = c(16))

modellist1 <- list()
for (ntree in seq(100, 2000, by=100)) {  
  set.seed(100003)
  fit1 <- train(x = x_train1, y = y_train1, method="rf",
               metric="Accuracy", tuneGrid=grid4,
               trControl=ctrl1, ntree=ntree)
  key1 <- toString(ntree)
  modellist1[[key1]] <- fit1
}

results1 <- resamples(modellist1)
summary(results1)

bwplot(results1)
dotplot(results1)  
densityplot(results1)  


set.seed(250209)
fit_rf_clsm1 <- randomForest(x = x_train1,
                            y = y_train1,
                            mtry = 16,
                            ntree = 500,
                            importance = T)

print(fit_rf_clsm1)

# "ERROR & TREES"
plot(fit_rf_clsm1,main = "ERROR & TREES")
#
plot(randomForest::margin(fit_rf_clsm1), main = '')

# predict
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


# x_test1 <- genus_test_top[, -11]  
# y_test1 <- as.factor(genus_test_top[, 11])  
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

multiClassSummary(
  data.frame(obs = genus_test_top$class,pred=testpredlab),
  lev = levels(genus_test_top$class)
)

results <- data.frame(Actual = genus_test_top$class, Predicted = testpredlab)


confusion_matrix_df <- as.data.frame.matrix(confusion_matrix$table)

# saving
saveRDS(fit_rf_clsm, 
 file = "")

# working

# loading
fit_rf_clsm1 <- readRDS("")

# 
testdata1 <- read.csv(file.choose())
testdata1 <- as.data.frame(testdata1)
for (i in c(1:2)) {
  testdata1[,i] <- factor(testdata1[,i])
}


testpredprob <- predict(fit_rf_clsm1, 
                        newdata = testdata1, 
                        type = "prob")

multiclass.roc(response = testdata1$class, 
               predictor = testpredprob)

testpredlab <- predict(fit_rf_clsm1, 
                       newdata = testdata1,
                       type = "class")

confusion_matrix <- confusionMatrix(data = testpredlab,
                reference = testdata1$class,
                mode = "everything")

multiClassSummary(
  data.frame(obs = testdata1$class,pred=testpredlab),
  lev = levels(testdata1$class)
)

results <- data.frame(Actual = testdata1$class, Predicted = testpredlab)
