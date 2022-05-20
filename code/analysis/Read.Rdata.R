rm(list=ls()) #clear the working directory
library(plyr)
library(Hmisc)
library(tidyverse)

#open data files
setwd("/Users/nathaliedusart/Desktop/RPEP/experiment")
input <- data.frame()
setwd("/Users/nathaliedusart/Desktop/RPEP/experiment/data/raw")
files <- dir(pattern = "CardID_")

for (i in files) {
  tmp <- read.csv(i, header = TRUE) #read the file
  
  #add the content to the data frame
  input <- rbind(input,tmp)
  rm(tmp)
}

#filter out UPPS items
UPPS <- filter(input, outcome == 'undefined')
input <- filter(input, resp == 'undefined')

#creating new variables
input$trial_number <- as.numeric(input$trial_number)
input$Participant <- factor(input$prolific_ID)
arrange(input, Participant, trial_number)
input$X.2 <- NULL
input$X.1 <- NULL
input$X <- NULL

input$prevOut <- c('NA', input$outcome[1: nrow(input)-1]) #outcome of the previous trial


#correcting the format of some variables + adding some variables
input$startRT <- as.numeric(input$startRT)
input$RT <- as.numeric(input$RT)
input$startPrev <- lag(input$startRT)
input$startDiff <- input$startRT - input$startPrev


#extra factors
input$Feelings <- factor(input$Feelings)
input$outcome <- factor(input$outcome)
input$prevOut <- factor(input$prevOut)
input$valence <- factor(input$block_number)

#check if overall design worked
table(input$Participant, input$outcome)
table(input$Participant, input$RT)

#exclude subjects
subject_excl <- c(23) #twice as much trials
input <- subset(input, Participant %nin% subject_excl)

#remove trials with overly long RT + very first trial 
before <- nrow(input)
input <- subset(input, RT < 2500)
input <- subset(input, startRT < 5000)
input <- subset(input, trial_number != 1)
input <- subset(input, prevOut != 'NA')
after <- nrow(input)
removed <- 1 - (after/before)
removed

#write everything to a file
input$Participant <- droplevels(input$Participant)
input$prevOut <- droplevels(input$prevOut)

save(input, file = "/Users/nathaliedusart/Desktop/RPEP/experiment/data/processed/input.Rdata")
save(UPPS, file = "/Users/nathaliedusart/Desktop/RPEP/experiment/data/processed/UPPS.Rdata")






