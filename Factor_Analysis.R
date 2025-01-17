# See 
https://towardsdatascience.com/exploratory-factor-analysis-in-r-e31b0015f224
 and 
https://www.uwo.ca/fhs/tc/labs/10.FactorAnalysis.pdf
; 
https://livebook.manning.com/book/r-in-action/chapter-14/27
; 

# Install and load needed packages;

if (!require(psych)) install.packages('psych'); library(psych);

if (!require(plfm)) install.packages('plfm'); library(plfm);

if (!require(corrplot)) install.packages('corrplot'); library(corrplot);

if (!require(ggplot2)) install.packages('ggplot2'); library(ggplot2);

if (!require(car)) install.packages('car'); library(car);

# Get data;

car1 <- data.frame(car$freq1);

df <- car1;

# Examine correlation matrix;

datamatrix <- cor(df); corrplot(datamatrix, method="number");

# Check suitability for factorization;

KMO(r = cor(df));

X <- df;

library(ggplot2);

fafitfree <- fa(dat,nfactors = ncol(X), rotate = "none");

n_factors <- length(fafitfree$e.values);

scree     <- data.frame(

+   Factor_n =  as.factor(1:n_factors), 

+   Eigenvalue = fafitfree$e.values)

> ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 

+   geom_point() + geom_line() +

+   xlab("Number of factors") +

+   ylab("Initial eigenvalue") +

+   labs( title = "Scree Plot", 

+         subtitle = "(Based on the unreduced correlation matrix)")

parallel <- fa.parallel(X)

# Create and display factor analysis;

 fa.none <- fa(r=X, nfactors = 5, covar = FALSE, SMC = TRUE, fm="pa",max.iter=100, rotate="varimax"); 

 fa.diagram(fa.none);
