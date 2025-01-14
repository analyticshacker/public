# Instructions:  Paste each block of code into R, one at a time, to see what it does;

# Supervised learning for continuous dependent variable (“regression”)

# See tutorial at 
https://www.machinelearningplus.com/machine-learning/caret-package/
  ; 

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

if(!require(earth)) install.packages('earth'); library(earth);

# Set random number generator seed to get reproducible results;

set.seed(1);

# Read data; 

data <- read.csv('Downloads/mall_Customers.csv');

# Clean data;

## drop the customer id column;

data <- data[,2:5];

## Rename the column names;

colnames(data) <- c("Gender","Age","Income","Score");

## Check for any na values ;

colSums(is.na(data));

## Create dummy column "ScoreCategory" for classification;

data <- data %>% 
  
  mutate(ScoreCategory = ntile(Score, 4));

data <- data %>%
  
  mutate(ScoreCategory = case_when(ScoreCategory == '1'~ 'verylow',
                                   
                                   ScoreCategory == '2' ~ 'low',
                                   
                                   ScoreCategory == '3'~ 'medium',
                                   
                                   ScoreCategory == '4' ~ 'high'));

data$Gender <- as.factor(data$Gender);

data$ScoreCategory <- as.factor(data$ScoreCategory);

#  Partition data into Training and Testing sets; 

trainIndex <- createDataPartition(data$Score, p = .8, 
                                  
                                  list = FALSE, 
                                  
                                  times = 1); 

dataTrain <- data[trainIndex,]; 

dataTest <- data[-trainIndex,]; 

# K-fold cross-validation; 

fitControl <- trainControl(## 10-fold CV; 
  
  method = "repeatedcv"
  
  ,number = 10
  
  ,repeats = 10
  
  ,savePredictions=TRUE
  
  ,classProbs=FALSE
  
  ,allowParallel = TRUE
  
); 

# Regression models (may take time); 

algorithmList <- c('rf', 'rpart', 'knn', 'gbm', 'earth', 'lm', 'svmRadial'); 

regModels <- caretList(Score ~ Gender+Age+Income, data=data, 
                       
                       trControl=fitControl, 
                       
                       methodList=algorithmList) ; 

regResults <- resamples(regModels);

# Show results;

summary(regResults);

#dot plots to compare models; 

regScales <- list(x=list(relation="free"), y=list(relation="free")); 

dotplot(regResults, scales=regScales); 

