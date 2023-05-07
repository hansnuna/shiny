library(tidyverse)
library(catboost)
library(caret)
library(tidyverse)
library(magrittr)
library(caTools)

setwd("C:/Koweps")
suicpre <- read.csv("cleaned0507.csv", header=TRUE)

# ------- prepare data set ------ #

# generate the undersampling data by suicidality
suicpre$suic_tght <- as.factor(suicpre$suic_tght)

# declare variable type
cols <- c("lostpr", "divpr", "dropsch", "liverel", "edu_fa", "edu_mo", "job_fa", "job_mo",
          "inher", "immgr", "edu_ca", "marr_ca", "prov",
          "relig", "emp_ca", "chronc", "smk", "lvcond1",
          "lvcond2", "lvcond3", "lvcond4", "lvcond5", "lvcond6", "lvcond7", "lvcond8",
          "lvcond9", "lvcond10", "lvcond11", "lvcond12", "lvcond13")

suicpre %<>% mutate_at(cols, factor)
suicpre$cesd <- as.numeric(suicpre$cesd)
suicpre$selfes <- as.numeric(suicpre$selfes)

str(suicpre)

#subset unnecessary features
suicpre <- subset(suicpre, select=-c(1,2))
suicpre <- subset(suicpre, select=-c(56))
# suicpre <- subset(suicpre, select=c(29, 30, 57))
#predictors <- suicpre[, -c(57, 58)]
#response <- suicpre$suic_tght

###################################################
########### for suicidal ideation #################

# split the dataset into training and test
set.seed(123)
split = sample.split(suicpre$suic_tght, SplitRatio = 0.7)

train_df <- subset(suicpre, split == T)
test_df <- subset(suicpre, split == F)

library(ROSE)
train_b <- ovun.sample(suic_tght~.,
                       data=train_df,
                       seed=123,
                       method="under")$data

ft_train <- train_b[-55]
tg_train <- as.numeric(train_b$suic_tght)


test_b <- ovun.sample(suic_tght~.,
                       data=test_df,
                       seed=123,
                       method="under")$data
ft_test <- test_b[-55]
tg_test <- as.numeric(test_b$suic_tght)

train_set <- catboost.load_pool(data = ft_train, label = tg_train)
test_set <- catboost.load_pool(data = ft_test, label = tg_test)

# define the model parameters
params <- list(iterations = 1000, learning_rate = 0.01, depth = 8, loss_function = "Logloss", eval_metric = "AUC")


# cross-validation
set.seed(123)
cb_cv <- catboost.cv(train_set, params = params, fold_count = 5, partition_random_seed = 0,
                     shuffle = T, stratified = T)

best_value = min(cb_cv["test.Logloss.mean"])  #0.4168555(0.05, 8) / ###0.411411 (0.01, 8)### / 0.412284(0.01, 10)
best_value


# build the model with best params
params <- list(iterations = 946, learning_rate = 0.01, depth = 8, loss_function = "Logloss", eval_metric = "AUC")

# train the model
myModel <- catboost.train(train_set, test_set, params)

#test the model
preds <- catboost.predict(myModel, test_set, prediction_type = "Probability")
preds

library(pROC)
# ROC
roc_cb <- roc(test_b$suic_tght, preds)
plot.roc(roc_cb, legacy.axes = T, print.auc = T, print.thres = "best", asp = NA)
coords(roc_cb, "best", ret = "threshold", transpose = F)

# AUC
auc(roc_cb)

# confusion matrix at best threshold
cutoff <- as.numeric(coords(roc_cb, "best", ret="threshold", transpose = FALSE))
confm <- table(preds > cutoff, test_b$suic_tght)
confm

### accuracy ###
(confm[1,1] + confm[2,2]) / sum(confm)

### sensitivity ###
TPR <- confm[2,2] / (confm[1,2] + confm[2,2])
TPR

### specificity ###
TNR <- confm[1,1] / (confm[1,1] + confm[2,1])
TNR

# feature importance
imp <- catboost.get_feature_importance(myModel)
imp <- round(imp, 4)
impDf <- data.frame(feature = colnames(ft_test), importance = imp)
impDf <- impDf[impDf[,2]>3, ]

ggplot(data = impDf, aes(x = feature, y = importance, fill = feature)) +
  geom_bar(stat = "identity")

###### reduced model ########

# keep the two important features
suicpre <- subset(suicpre, select=c(27, 28, 55))

# split the dataset into training and test
set.seed(123)
split = sample.split(suicpre$suic_tght, SplitRatio = 0.7)

train_df <- subset(suicpre, split == T)
test_df <- subset(suicpre, split == F)

library(ROSE)
train_b <- ovun.sample(suic_tght~.,
                       data=train_df,
                       seed=123,
                       method="under")$data

ft_train <- train_b[-3]
tg_train <- as.numeric(train_b$suic_tght)


test_b <- ovun.sample(suic_tght~.,
                      data=test_df,
                      seed=123,
                      method="under")$data
ft_test <- test_b[-3]
tg_test <- as.numeric(test_b$suic_tght)

train_set <- catboost.load_pool(data = ft_train, label = tg_train)
test_set <- catboost.load_pool(data = ft_test, label = tg_test)

# define the model parameters
params <- list(iterations = 1000, learning_rate = 0.05, depth = 5, loss_function = "Logloss", eval_metric = "AUC")


# cross-validation
set.seed(123)
cb_cv <- catboost.cv(train_set, params = params, fold_count = 5, partition_random_seed = 0,
                     shuffle = T, stratified = T)

best_value = min(cb_cv["test.Logloss.mean"])  #0.4262636(0.05, 8) / 0.4268307 (0.01, 8) / 0.4284418(0.05, 10) / ####0.4251452(0.05, 5) / 0.4253971 (0.05, 3)
best_value


# build the model with best params
params <- list(iterations = 49, learning_rate = 0.05, depth = 5, loss_function = "Logloss", eval_metric = "AUC")

# train the model
myModel <- catboost.train(train_set, test_set, params)

#test the model
preds <- catboost.predict(myModel, test_set, prediction_type = "Probability")
preds

# ROC
roc_cb <- roc(test_b$suic_tght, preds)
plot.roc(roc_cb, legacy.axes = T, print.auc = T, print.thres = "best", asp = NA)
coords(roc_cb, "best", ret = "threshold", transpose = F)

# AUC
auc(roc_cb)

# confusion matrix at best threshold
cutoff <- as.numeric(coords(roc_cb, "best", ret="threshold", transpose = FALSE))
confm <- table(preds > cutoff, test_b$suic_tght)
confm

### accuracy ###
(confm[1,1] + confm[2,2]) / sum(confm)

### sensitivity ###
TPR <- confm[2,2] / (confm[1,2] + confm[2,2])
TPR

### specificity ###
TNR <- confm[1,1] / (confm[1,1] + confm[2,1])
TNR

saveRDS(myModel, file = "catmodel.rda")

