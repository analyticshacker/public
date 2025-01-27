# Individual conditional expectation (ICE) plots;

# Please make sure to run the earlier PDP script (previous script) first;

pdp <- partial(rf, pred.var = c("Advertising"), ice = T);

autoplot(pdp, ylab = "Sales");

# Centered ICE plots;

pdp <- partial(rf, pred.var = c("Advertising"), ice = T, center = T);

autoplot(pdp, ylab = "Sales");

# Create better-looking plot;

autoplot(pdp, ylab = "Sales") + theme(axis.title.y = element_text(size = rel(2), angle = 90)) + theme(axis.title.x = element_text(size = rel(2), angle = 0))+ theme(axis.text.x = element_text(angle = 0, hjust = 1, size=18,color="darkred")) +  theme(axis.text.y = element_text(angle = 0, hjust = 1, size=18,color="darkred")) + geom_point(size = 3);

# Individual conditional expectation (ICE) plot clusters;

if (!require(ICEbox)) install.packages('ICEbox'); 

library(pdp); library(ggfortify); library(randomForest); library(ISLR); attach(Carseats);

library(ICEbox); X = Carseats; y = X$Sales;

Sales.rf = randomForest(X, y); 

## Create an 'ice' object for the predictor "Advertising";

Sales.ice = ice(object = Sales.rf, X = X, y = y, predictor = "Advertising", frac_to_build = .5) 

## cluster the curves into 3 groups, start all at 0;

clusterICE(Sales.ice, nClusters = 3, plot_legend = TRUE, center = TRUE);

#2D-Partial dependence plot (PDP);

if (!require(pdp)) install.packages('pdp'); if (!require(ggfortify)) install.packages('ggfortify'); 

library(pdp); library(ggfortify); library(randomForest); library(ISLR); attach(Carseats);

rf <- randomForest(Sales ~ ., data = Carseats);

pdp <- partial(rf, pred.var = c("Advertising", "Age"), chull = TRUE);

autoplot(pdp);

plotPartial(pdp); # plotPartial is an alternative to autoplot for plotting the 2D-PDP;

# Create better-looking plot;

autoplot(pdp) + theme(axis.title.y = element_text(size = rel(2), angle = 90)) + theme(axis.title.x = element_text(size = rel(2), angle = 0))+ theme(axis.text.x = element_text(angle = 0, hjust = 1, size=18,color="darkred")) +  theme(axis.text.y = element_text(angle = 0, hjust = 1, size=18,color="darkred"));
