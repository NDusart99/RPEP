rm(list = ls())
library(reshape)
library(ez)
library(Hmisc)
library(ggplot2)
library(doBy)
library(reshape2)

#Loading data
load(file = "/Users/nathaliedusart/Desktop/analyse/Data/processed/input.Rdata")
load(file = "/Users/nathaliedusart/Desktop/analyse/Data/processed/UPPS.Rdata")

#melt the behavioral data and UPPS data
input.melt <- melt(input,
                   id.vars = c('Participant', 'prevOut'),
                   measure.vars = c('startRT'))

#exclude the same subjects as in the input
UPPS <- subset(UPPS, prolific_ID %in% input$prolific_ID)

UPPS$resp <- as.numeric(UPPS$resp)
UPPS.melt <- melt(UPPS,
                  id.vars = c('prolific_ID', 'factor'),
                  measure.vars = c('resp'))

#combine behavioral data and UPPS
#Data frame of beh. data
corr <- cast(input.melt, Participant ~ prevOut, mean, subet = variable == "startRT")
corrUPPS <- cast(UPPS.melt, prolific_ID ~ factor, sum, subset = variable == "resp")

#calculate the difference scores for nongamble vs loss, nongamble vs win and win vs loss
corr$diffNGL <- (corr$nonGamble - corr$gambleLoss)/corr$nonGamble
corr$diffNGW <- (corr$nonGamble - corr$gambleWin)/corr$nonGamble
corr$diffWL <- (corr$gambleWin - corr$gambleLoss)/corr$nonGamble
corr$NU <- corrUPPS$'Negative Urgency'
corr$PU <- corrUPPS$'Positive Urgency'
corr$sensation <- corrUPPS$'Sensation Seeking'
corr$premeditation <- corrUPPS$Premeditation
corr$perseverance <- corrUPPS$Perseverance

corr <- data.frame(corr[,5:12])
rcorr(as.matrix(corr), type=c("pearson"))

#ggplot(corr, aes(bet, diff)) + geom_point() + geom_smooth(method = 'lm', alpha = 0.1, fill = 'blue')

exp.corr <- corr
save(exp.corr, file = "/Users/nathaliedusart/Desktop/analyse/Data/processed/exp_corr.Rdata")



