# Bayesian networks;

# Instructions:  Paste each block of code into R, one at a time, to see what it does;

# Install and load needed packages.;

if (!require(bnlearn)) install.packages('bnlearn');

library(bnlearn);

# Attach the Carseats data set;

library(ISLR); attach(Carseats);

# Learn BN network structure from data using default settings in bnlearn package;

# NOTE:  The following code gives a somewhat different BN network structure from the one used in class, because it does not modify the default settings. Some of the arrows (such as the one from Advertising to Population) have the wrong directions;

# Learn BN network using hill-climbing (hc) causal discovery algorithm; 

bn <- hc(Carseats);

# View the BN network;

graphviz.plot(bn);

# Edit the BN network and view the results;

bn <- reverse.arc(bn, from = "Advertising", to = "Population");

graphviz.plot(bn);

# Fit conditional probability tables or models (CPTs) to data;

BN <- bn.fit(bn, Carseats);

# Predict the conditional probability that Sales > 7 given that Advertising < 8;

cpquery(BN, event = (Sales > 8), evidence = (Advertising < 8));
