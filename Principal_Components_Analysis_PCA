# Install and load plfm package and car dataset;

if (!require(plfm)) install.packages('plfm'); library(plfm);data(car);

car1 <- data.frame(car$freq1); df <- car1;

# Create and display Principal Components biplot;

pc <- prcomp(df, scale = T);

summary(pc);

biplot(pc, cex = 1.6, cex.lab = 1.6, cex.axis = 1.6, pch = 16);  
