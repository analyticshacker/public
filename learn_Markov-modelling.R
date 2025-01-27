# Markov modeling;

# Code created by Vishnu Vardhan Guttikonda, University of Colorado 

# Instructions:  Paste each block of code into R, one at a time, to see what it does;

# Importing Libraries

if(!require(markovchain)) install.packages('markovchain'); library(markovchain)

if(!require(ggplot2)) install.packages('ggplot2'); library(ggplot2)

if(!require(gganimate)) install.packages('gganimate'); library(gganimate)

if(!require(gifski)) install.packages('gifski'); library(gifski)

if(!require(viridis)) install.packages('viridis'); library(viridis)

if(!require(hrbrthemes)) install.packages('hrbrthemes'); library(hrbrthemes)

if(!require(tidyr)) install.packages('tidyr'); library(tidyr)

if(!require(diagram)) install.packages('diagram'); library(diagram)

if(!require(transformr)) install.packages('transformr'); library(transformr)

if(!require(plyr)) install.packages('plyr');library(plyr)

if(!require(DescTools)) install.packages('DescTools');library(DescTools)

if(!require(strex)) install.packages('strex');library(strex)

if(!require(gdata)) install.packages('gdata');library(gdata)

if(!require(ChannelAttribution)) install.packages('ChannelAttribution');library(ChannelAttribution)

if(!require(reshape2)) install.packages('reshape2');library(reshape2)

if(!require(zeallot)) install.packages('zeallot'); library(zeallot)

## Custom class to define an object

custom.markov.class <- setClass("custom.markov.class", ## any name can be given

                                slots=list(nstates="numeric", 

                                           state.names ="character", 

                                           trans.matrix="matrix",

                                           init.array = "array",

                                           iterations = "numeric",

                                           rescale.from.state = "character",

                                           rescale.to.state = "character",

                                           rescale.factor = "numeric",

                                           removaleffect.conv.state = "character",

                                           removaleffect.non.conv.state="character",

                                           removaleffect.start.state="character")

)

# Function to calculate the fraction/no of persons in each state after each step

fvals<-function(mchain,initialstate,n) {

  out<-data.frame()

  for (i in 0:n)

  {

    iteration<-initialstate*mchain^(i)

    rownames(iteration) <- paste("step:",i,sep="")

    out<-rbind(out,iteration)

  }

  out<-cbind(out, i=seq(0,n))

  return(out)

}

## Function to set the row and columns of the transition matrix

set.dimnames <- function(markov.obj){

  markov.obj@nstates <- length(markov.obj@state.names)

  dim(markov.obj@trans.matrix) <- c(markov.obj@nstates,markov.obj@nstates)

  markov.obj@trans.matrix <- t(markov.obj@trans.matrix)

  dimnames(markov.obj@trans.matrix) <- list(markov.obj@state.names,markov.obj@state.names) 

  return(markov.obj)

}

## Function to set the initial status of states represented in array

set.initarray <- function(markov.obj,input.list){

  if(isTRUE(length(input.list)==markov.obj@nstates)){

    markov.obj@init.array <- array(input.list, dim = c(1,markov.obj@nstates), dimnames=list("initial",markov.obj@state.names))

    return(markov.obj)

  }else{

    stop(paste('Error: dimension of input.list is not equal to',markov.obj@nstates))

    return(markov.obj)

  }

}

## Function to create new markov chain object

create.markov.chain.obj <- function(markov.obj){

  return(new("markovchain",

             states = markov.obj@state.names,

             byrow= TRUE,

             transitionMatrix = markov.obj@trans.matrix))

}

## function to plot markov chain transition diagram

mc.obj.plot <- function(mc.obj){

  return(plot(mc.obj, 

              package="diagram", 

              box.size = 0.09, 

              box.cex = 2,

              cex.txt = 2,

              curve=0.05,

              lwd = 1, 

              box.lwd = 2, 

              arr.length=.5,

              arr.width=.5,

              self.cex = .5,

              self.shifty = -.02,

              self.shiftx = .13)

  )

}

## Function to print the status of initial state vector after n steps

status.after.n.steps <- function(markov.obj){

  # Create new instance of markovchain object

  markov.chain.obj.new <- create.markov.chain.obj(markov.obj)

  #initial state is defined as Start = 1 and rest of the nodes have 0 fraction of persons

  steps<- fvals(mchain=markov.chain.obj.new,initialstate=markov.obj@init.array,n=markov.obj@iterations)  

  print(steps)

  # Converting into pivot for ggplot 

  steps_pivot <- steps %>% pivot_longer(c(names(subset(steps, select = -c(i)))), 

                                        names_to = "states", values_to = "values")

  # converting states to factor to maintain the order of legend in the plot 

  steps_pivot$states <- factor(steps_pivot$states, levels = c(names(subset(steps, select = -c(i)))))

  steps_pivot$states <- mapvalues(steps_pivot$states,markov.obj@state.names,markov.obj@state.names)

  # plot

  predictPlot <- ggplot(steps_pivot, aes(x = i, y = values, group = states, color = states)) +

    scale_x_discrete(limits = steps_pivot$i)+

    xlab('Chain Step') + 

    ylab('Probability') + 

    ggtitle(paste(markov.obj@iterations,' Step Chain Probability Prediction')) +

    geom_line(aes(color = states), size = 2) +

    geom_point(aes(color = states), size = 5) + 

    theme_minimal() +

    theme(text = element_text(size = 25)) 

  rm(markov.chain.obj.new)

  print(predictPlot)

}

## Function to plot the rescale of a transition

## Rescale: rescale of a particular probability between two states from 0 to 1 in steps

##        : rescaling will automatically adjust the other probabilities in that particular row to make the sum = 1

rescale.plot.save <- function(markov.obj){

  # Create new instance of markovchain object

  markov.chain.obj.new <- create.markov.chain.obj(markov.obj)

  rescaleTransProb <- function(inputTransMatrix,scaleFromState,scaleToState,scaleFactor){

    outTransMatrix <- as.matrix(inputTransMatrix)

    scaleValue <- outTransMatrix[scaleFromState,scaleToState]

    denomSum <- sum(outTransMatrix[scaleFromState, -1 * scaleToState])

    sumOfRow <- sum(outTransMatrix[scaleFromState,])

    if(denomSum==0) { 

      denomSum <- 1

      row <- outTransMatrix[scaleFromState,] + (sumOfRow - scaleFactor)/(length(outTransMatrix[scaleFromState,])-1)

    } else{

      row <- outTransMatrix[scaleFromState,] * (sumOfRow - scaleFactor)/denomSum 

    }

    row[scaleToState] <- scaleFactor

    outTransMatrix[scaleFromState,] <- row

    outTransMatrix <- as(outTransMatrix,"markovchain")

    return(outTransMatrix)

  }

  scaleFromState <- as.numeric(match(markov.obj@rescale.from.state,markov.obj@state.names))

  scaleToState <- as.numeric(match(markov.obj@rescale.to.state,markov.obj@state.names))

  rescaleSteps_pivot <- NA

  for (scaleFactor in seq(0,1,by = as.numeric(markov.obj@rescale.factor))) {

    rescaledTransMatrix <- rescaleTransProb(markov.chain.obj.new@transitionMatrix,scaleFromState,scaleToState,scaleFactor)

    rescaleSteps <- fvals(mchain=rescaledTransMatrix,initialstate=markov.obj@init.array,n=markov.obj@iterations)

    rescaleSteps["scaleFactor"] <- scaleFactor

    rescaleSteps_pivot <- rbind(rescaleSteps_pivot,rescaleSteps %>% pivot_longer(c(names(subset(rescaleSteps, select = -c(i, scaleFactor)))), 

                                                                                 names_to = "states", values_to = "values"))

  }

  rescaleSteps_pivot$states <- factor(rescaleSteps_pivot$states, levels = c(names(subset(rescaleSteps, select = -c(i, scaleFactor)))))

  rescaleSteps_pivot <- rescaleSteps_pivot[-1,]

  # plot

  rescaleSteps_pivot$states <- mapvalues(rescaleSteps_pivot$states,markov.obj@state.names,markov.obj@state.names)

  rescalePlot <- ggplot(rescaleSteps_pivot,

                        aes(x = i, y = values, group = interaction(states, scaleFactor), color = states)) +

    scale_x_discrete(limits = rescaleSteps_pivot$i)+

    xlab('Chain Step') + 

    ylab('Probability') + 

    ggtitle(paste(markov.obj@iterations,'Step Chain Probability Prediction - Transition Probability from state:',markov.obj@rescale.from.state,'to state:',markov.obj@rescale.to.state,' : ',"{format(round(frame_time, 2),nsmall = 2)}",sep=" ")) +

    geom_line(aes(color = states), size = 2) +

    geom_point(aes(color = states), size = 5) + 

    theme_minimal() +

    transition_time(scaleFactor) +

    theme(text = element_text(size = 25)) +

    ease_aes("linear")

  rm(markov.chain.obj.new)

  rm(rescaleSteps_pivot)

  return(animate(rescalePlot,

                 fps = 5,

                 width = 750,

                 height = 450,

                 renderer=gifski_renderer("rescalePlot.gif")))

}

## Function to calculate and plot the removal effect

removal.effect.create.path <- function(markov.obj){

  cnames <- c("StateName","ConvPath","Checked")

  finalPathDF <- data.frame(matrix(ncol=3,nrow = 0),stringsAsFactors=T)

  colnames(finalPathDF) <- cnames

  createPathDF <- function(markov.obj,conv.state,currentDF){

    mo.trans.matrix <- markov.obj@trans.matrix

    a <- which(mo.trans.matrix[,conv.state] > 0,arr.ind=T)

    for (name in names(a)){

      if(name == conv.state) {checked_flag <- "Y"} else {checked_flag <- "N"}

      k <- subset(finalPathDF,as.character(finalPathDF[,"StateName"]) == conv.state)

      if(dim(k)[1] > 0) {

        for (j in 1:dim(k)[1]) {

          txt <- as.character(k[j,"ConvPath"])

          if (is.na(txt)) { 

            txt <- conv.state

          }

          pathDF <- data.frame(name,paste(name,">",txt),checked_flag)

        }

      } else {

        pathDF <- data.frame(name,paste(name,">",conv.state),checked_flag)

      }

      colnames(pathDF) <- cnames

      currentDF <- rbind(currentDF,pathDF)

    }

    return(currentDF)

  }

  finalPathDF <- createPathDF(markov.obj,markov.obj@removaleffect.conv.state,finalPathDF)

  finalPathDF

  while(!all(as.character(finalPathDF$Checked) %in% c('Y'))){

    for ( i in 1:dim(finalPathDF)[1]){

      if(finalPathDF[i,"Checked"] == "N"){

        finalPathDF <- createPathDF(markov.obj,as.character(finalPathDF[i,1]),finalPathDF)

        finalPathDF[i,"Checked"] <- "Y"

      }

    }

  }

  finalPathDF <- unique(finalPathDF)

  row.names(finalPathDF) <- NULL

  finalPathDF

  finalPathDF <- createPathDF(markov.obj,markov.obj@removaleffect.non.conv.state,finalPathDF)

  finalPathDF

  while(!all(as.character(finalPathDF$Checked) %in% c('Y'))){

    for ( i in 1:dim(finalPathDF)[1]){

      if(finalPathDF[i,"Checked"] == "N"){

        finalPathDF <- createPathDF(markov.obj,as.character(finalPathDF[i,1]),finalPathDF)

        finalPathDF[i,"Checked"] <- "Y"

      }

    }

  }

  print(finalPathDF)

  finalPathDF["conv"] <- 0

  finalPathDF["conv_null"] <- 0

  finalPathDF[which(trim(str_after_last(as.character(finalPathDF[,"ConvPath"]),'>')) == markov.obj@removaleffect.conv.state),"conv"] <- 1

  finalPathDF[which(trim(str_after_last(as.character(finalPathDF[,"ConvPath"]),'>')) == markov.obj@removaleffect.non.conv.state),"conv_null"] <- 1

  finalPathDF <- finalPathDF[which(as.character(finalPathDF[,"StateName"]) != markov.obj@removaleffect.conv.state),]

  finalPathDF <- finalPathDF[which(as.character(finalPathDF[,"StateName"]) != markov.obj@removaleffect.non.conv.state),]

  finalPathDF <- unique(finalPathDF)

  row.names(finalPathDF) <- NULL

  finalPathDF <- finalPathDF[which(trim(str_before_first(as.character(finalPathDF[,"ConvPath"]),'>')) == markov.obj@removaleffect.start.state),]

  print(finalPathDF)

  H <- heuristic_models(finalPathDF, 'ConvPath', 'conv', var_value=NULL, sep = '>')

  M <- markov_model(finalPathDF, 'ConvPath', 'conv', var_value=NULL, order = 1) 

  R <- merge(H, M, by='channel_name')

  R1 <- melt(R, id='channel_name')

  total.conversions.plot <- ggplot(R1, aes(channel_name, value, fill = variable)) +

    geom_bar(stat='identity', position='dodge') +

    ggtitle('TOTAL CONVERSIONS') + 

    theme(axis.title.x = element_text(vjust = -2)) +

    theme(axis.title.y = element_text(vjust = +2)) +

    theme(title = element_text(size = 16)) +

    theme(plot.title=element_text(size = 20)) +

    theme(text = element_text(size = 25)) +

    ylab("")

  model_output <- markov_model(finalPathDF,

                               var_path = "ConvPath", 

                               var_conv= "conv",

                               var_null = "conv_null",

                               out_more = TRUE)

  print("Conversion & Non Conversion Path")

  print("-------------------------------------")

  print(finalPathDF)

  writeLines("\n")

  print("Total Conversions of Markov Model")

  print("-------------------------------------")

  print(model_output$result)

  writeLines("\n")

  print("Transition Probabilities of Markov Model")

  print("-------------------------------------")

  print(model_output$transition_matrix)

  writeLines("\n")

  print("Removal Effects from Markov Model")

  print("-------------------------------------")

  print(model_output$removal_effects)

  writeLines("\n")

  removal.effects.plot <- ggplot(model_output$removal_effects, 

                                 aes(channel_name, removal_effects, fill = channel_name)) +

    geom_bar(stat='identity', position='dodge') +

    ggtitle('Markov Model - Removal Effects') + 

    theme(axis.title.x = element_text(vjust = -2)) +

    theme(axis.title.y = element_text(vjust = +2)) +

    theme(title = element_text(size = 16)) +

    theme(plot.title=element_text(size = 20)) +

    theme(text = element_text(size = 25)) +

    ylab("")

  return(list(total.conversions.plot,removal.effects.plot,finalPathDF,model_output))

}

# Create new object for Markov chain

markov.obj <- new("custom.markov.class")

markov.obj@state.names <- c('Start', 'Open Email','Visit Website','Demo Request','Buy','Lost')

markov.obj@trans.matrix <- matrix(c(0,0.8,0.2,0.0,0.0,0.0,

                                    0,0.0,0.4,0.0,0.0,0.6,

                                    0,0.0,0.0,0.9,0.1,0.0,

                                    0,0.0,0.0,0.0,0.7,0.3,

                                    0,0.0,0.0,0.0,1.0,0.0,

                                    0,0.0,0.0,0.0,0.0,1.0))

markov.obj <- set.dimnames(markov.obj)

markov.obj <- set.initarray(markov.obj, c(1,0,0,0,0,0))

print(markov.obj)

# Creating markovchain object

mc.obj <- create.markov.chain.obj(markov.obj)

mc.obj@transitionMatrix

# plotting transition matrix 

# plotting transition matrix 

mc.obj.plot(mc.obj)

## plot the status of initial vector after n iterations

markov.obj@iterations = 10

status.after.n.steps(markov.obj)

#  Rescale

# ============

# Rescale any transition probability from 0 to 1 with rescale.factor(.01) steps

markov.obj@rescale.from.state = "Open Email"

markov.obj@rescale.to.state = "Buy"

markov.obj@rescale.factor = 0.01

markov.obj@iterations = 10

rescale.plot.save(markov.obj)

## RemovalEffect;

markov.obj@removaleffect.conv.state = "Buy";

markov.obj@removaleffect.non.conv.state = "Lost";

markov.obj@removaleffect.start.state = "Start";

c(total.conversions.plot, removal.effects.plot, final.path.DF,model.output) %<-% removal.effect.create.path(markov.obj);

total.conversions.plot;

removal.effects.plot;
