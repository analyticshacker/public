# Partial dependence plot (PDP);

# Instructions:  Paste each block of code into R, one at a time, to see what it does;

# Install and load needed packages.

if (!require(pdp)) install.packages('pdp'); if (!require(ggfortify)) install.packages('ggfortify'); 

library(pdp); library(ggfortify); library(randomForest); 

# Attach the Carseats data set;

library(ISLR); attach(Carseats);

# Create random forest model;

rf <- randomForest(Sales ~ ., data = Carseats);

# Create partial dependence plot (PDP) of Sales vs. Advertising;

pdp <- partial(rf, pred.var = c("Advertising")); autoplot(pdp, ylab = "Sales ");

# Create better-looking plot;

autoplot(pdp, ylab = "Sales") + theme(axis.title.y = element_text(size = rel(2), angle = 90)) + theme(axis.title.x = element_text(size = rel(2), angle = 0))+ theme(axis.text.x = element_text(angle = 0, hjust = 1, size=18,color="darkred")) +  theme(axis.text.y = element_text(angle = 0, hjust = 1, size=18,color="darkred")) + geom_point(size = 3);

# Calculate and display multiple PDPs;

library(ISLR);  attach(Carseats); library(ggplot2); library(randomForest); library(pdp);

Carseats.rf <- randomForest(Sales ~ ., data = Carseats, mtry = 5, importance = TRUE);

top4<- c("Price", "CompPrice", "Advertising", "Income");

# Construct partial dependence functions for top four predictors; 

pd <- NULL;

 for (i in top4) { tmp <- partial(Carseats.rf, pred.var = i);

 names(tmp) <- c("x", "y");

 pd <- rbind(pd, cbind(tmp, predictor = i)) };

 # Display partial dependence functions;

 ggplot(pd, aes(x, y)) + geom_line() + facet_wrap(~ predictor, scales = "free") + theme_bw() + ylab("Sales");
