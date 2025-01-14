# This R code performs a benchmark analysis of various machine learning models to predict customer churn for a telecom company.

# See tutorial at https://www.machinelearningplus.com/machine-learning/caret-package/; 

# Load needed packages;

if(!require(caret)) install.packages('caret'); library(caret);

if(!require(rattle)) install.packages('rattle'); library(rattle);

if(!require(caretEnsemble)) install.packages('caretEnsemble'); library(caretEnsemble);

if(!require(randomForest)) install.packages('randomForest'); library(randomForest);

if(!require(gbm)) install.packages('gbm'); library(gbm);

if(!require(e1071)) install.packages('e1071'); library(e1071);

if(!require(klaR)) install.packages('klaR'); library(klaR);

if(!require(naivebayes)) install.packages('naivebayes'); library(naivebayes);

if(!require(kernlab)) install.packages('kernlab'); library(kernlab);

if(!require(dplyr)) install.packages('dplyr'); library(dplyr);

if(!require(ggplot2)) install.packages('ggplot2'); library(ggplot2);

if(!require(scales)) install.packages('scales'); library(scales);

if(!require(cowplot)) install.packages('cowplot'); library(cowplot);

if(!require(DT)) install.packages('DT'); library(DT);

if(!require(mltest)) install.packages('mltest'); library(mltest);

if(!require(rpart)) install.packages('rpart'); library(rpart);

if(!require(rpart.plot)) install.packages('rpart.plot'); library(rpart.plot);

if(!require(earth)) install.packages('earth'); library(earth);

if(!require(LogicReg)) install.packages('LogicReg'); library(LogicReg)

if(!require(stringr)) install.packages('stringr'); library(stringr);

if(!require(tidyverse)) install.packages('tidyverse'); library(tidyverse);

if(!require(Rmisc)) install.packages('Rmisc'); library(Rmisc);

# Set random number generator seed to get reproducible results;

set.seed(1);

# Get data;

# Read Telecom Churn data from Documents or other file and name it “tc”. Data source: https://www.kaggle.com/skanderhaddad/churn-prection-with-churn-score-feature/data;

tc <- read.csv("TelcoChurn.csv");

# Clean data;

# Convert all character columns to factors (discrete variables with levels);

tc <- as.data.frame(unclass(tc), stringsAsFactors = TRUE);                  

# Remove customer ID column;

tc <-tc[-1];

# Remove 3 records with missing data;

dim(tc);

tc <- na.omit(tc);

dim(tc); 

## Remove columns with only one unique value;

tc <- subset(tc, select = -c(which(sapply(tc, function(x) (is.character(x) | is.factor(x)) & length(unique(x))<2))));

# Prepare training and test sets;

# Partition the data into 70% training set and 30% test set;

train.flag <- createDataPartition(y= tc$Churn, p = 0.7, list=FALSE);

data.training <- tc[train.flag,];

data.testing <- tc[-train.flag,];

# Compare multiple classification models;

# K-fold cross-validation; 

fitControl <- trainControl(## 10-fold CV; 

  method = "repeatedcv"

  ,number = 10

  ,repeats = 3

  ,savePredictions=TRUE

  ,classProbs=FALSE

  ,allowParallel = TRUE

); 

## Classification models

## classification models - rf, svm, rpart

## takes 5 to 10 mins to execute the below 

algorithmList <- c("rf","svmRadial","rpart");

class.models <- caretList( Churn ~. ,

                           data=data.training, 

                           trControl=fitControl, 

                           methodList=algorithmList);

## x (predictors) and Y (response) for Logistic and NaiveBayes;

x.train <- subset(data.training,select = -c(Churn));

Y.train <- data.training$Churn;

x.test <- subset(data.testing,select = -c(Churn));

Y.test <- data.testing$Churn;

### classification model - logistic regression;

class.models$glm <- train( x = x.train,

                    y = Y.train,

                    trControl=fitControl, 

                    method = "glm",

                    family = "binomial");

### classification model - naive bayes;

class.models$nb <- train(x = x.train, 

                  y = Y.train, 

                  trControl=fitControl, 

                  method = "nb");

class.results <- resamples(class.models);

#### Show results;

## summary(class.results);

summary.results <- t(summary(class.results$values));

confidence.results <- format(t(sapply(class.results$values, CI)),decimal.mark = ".",digits = 2);

total.results <- cbind(summary.results,confidence.results[,c("lower","upper")])[-1,];

total.results <- as.data.frame(total.results);

colnames(total.results) <- c("Min","1st Qu", "Median","Mean","3rd Qu","Max","Lower-0.95","Upper-0.95");

total.results$Min <- sub(".*:", "", total.results$Min);

total.results$`1st Qu` <- sub(".*:", "", total.results$`1st Qu`);

total.results$Median <- sub(".*:", "", total.results$Median);

total.results$Mean <- sub(".*:", "", total.results$Mean);

total.results$`3rd Qu` <- sub(".*:", "", total.results$`3rd Qu`);

total.results$Max <- sub(".*:", "", total.results$Max);

## Re-order the output results;

total.results <- total.results[ order(sub(".*~", "", rownames(total.results)), desc(total.results$Mean)),];

## Split the results for display purpose;

total.results <- split(total.results,sub(".*~", "", rownames(total.results)));

# dot plots to compare models;

class.scales <- list(x=list(relation="free"), y=list(relation="free"));

dotplot(class.results, scales=class.scales);

## Plot confusion Matrix ;

ggplotConfusionMatrix <- function(m,mname){

  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),

                   "Kappa", percent_format()(m$overall[2]),

                   " - ",

                   mname);

  p <-

    ggplot(data = as.data.frame(m$table) ,

           aes(x = Reference, y = Prediction)) +

    geom_tile(aes(fill = log(Freq)), colour = "white") +

    scale_fill_gradient(low = "#FBB8AA",high = "#AADBFB") +

    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +

    theme(legend.position = "none") +

    ggtitle(mytitle)

  return(p)

};

algorithmList =  c("rf","svmRadial","rpart","glm",'nb');

plots <- vector("list",length = length(algorithmList));

classifier_metrics <- vector("list",length = length(algorithmList));

metrics_table <- vector("list",length = length(algorithmList));

classifier_metrics_table <- data.frame();

for (i in 1:length(algorithmList)){

  true_values <- as.factor(data.testing$Churn)

  predicted_values <- as.factor(predict(class.models[i], data.testing))

  cfm <- confusionMatrix(predicted_values,true_values)

  plots[[i]] <- ggplotConfusionMatrix(cfm,algorithmList[i])

  classifier_metrics[[i]] <- ml_test(predicted_values, true_values, output.as.table = TRUE)

  metrics_table[[i]] <- t(as.data.frame(classifier_metrics[i]))

  colnames(metrics_table[[i]]) <- paste(algorithmList[i],'.',colnames(metrics_table[[i]]), sep= "")

};

## Plot confusion matrices ;

plot_grid(plotlist=plots)  ;

## Print final results;

print("Final Results");

print(total.results);

