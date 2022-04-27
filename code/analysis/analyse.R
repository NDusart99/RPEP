rm(list = ls())
library(reshape)
library(ez)
library(Hmisc)
library(ggplot2)
library(doBy)
library(reshape2)
library(tidyverse)
library(dplyr)

load(file = "/Users/nathaliedusart/Desktop/RPEP/experiment/data/processed/input.Rdata")
load(file = "/Users/nathaliedusart/Desktop/RPEP/experiment/data/processed/UPPS.Rdata")

##prepare the data, adjust choice value & create melted object variables
#melt the data
input.melt <- melt(input,
                   id.vars = c('Participant', 'prevOut', 'Feelings', 'valence'),
                   measure.var = c('startRT', 'RT'))
input.melt$Feelings <- factor(input.melt$Feelings, levels = c("Unhappy", "Somewhat unhappy", "Somewhat happy", "Happy"))
input.melt$prevOut <- factor(input.melt$prevOut)
input.melt$valence <- factor(input.melt$valence, levels = c("No music", "Negative valence", "Positive valence"))

inputsub <- filter(input, valence == 'Negative valence' | valence == 'Positive valence')


inputsub.melt <- melt(inputsub, 
                      id.var = c('Participant', 'prevOut', 'valence', 'Feelings'), 
                      measure.var = c('startRT', 'RT'))
inputsub.melt$Feelings <- factor(inputsub$Feelings, levels = c("Unhappy", "Somewhat unhappy", "Somewhat happy", "Happy"))
inputsub.melt$valence <- factor(inputsub.melt$valence, levels = c("No music", "Negative valence", "Positive valence"))


#analysis of the UPPS score
UPPS$resp <- as.numeric(UPPS$resp)
UPPS.melt <- melt(UPPS,
                  id.vars = c('prolific_ID', 'factor'),
                  measure.vars = c('resp'))

#startRT distribution
ggplot(input,aes(x = trial_number, y = startRT)) + stat_summary(fun = mean, geom = "point")

#RT distribution
ggplot(input, aes(x = trial_number, y = RT)) + stat_summary(fun = mean, geom = "point")

#### Effect of previous outcome ####
start.cast <- cast(input.melt, Participant * prevOut ~ ., mean, subset = variable == "startRT")
names(start.cast)[3] <- "startRT"

#plot the interaction
plot <- ggplot(start.cast, aes(x = prevOut, y = startRT)) + 
  stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.10)

Cardsplot <- plot + coord_cartesian(ylim = c(400,800)) + theme_bw() + labs (x = "Previous Outcome", 
                                                                            y = "Start RT (in ms)") +
  scale_x_discrete(labels = c("nonGamble" = "Non-gamble", "gambleLoss" = "Gambled Loss", "gambleWin" = "Gambled Win")) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

Cardsplot

summaryBy(startRT ~ prevOut, data= as.data.frame(start.cast), FUN = c(mean, sd))
startRTplot <- Cardsplot
save(startRTplot, file = "/Users/nathaliedusart/Desktop/RPEP/experiment/data/processed/prevOut_plot.Rdata")

#Do an ANOVA
output <- ezANOVA(
  data = start.cast
  , dv = .(startRT)
  , wid = .(Participant)
  , within = .(prevOut)
  , detailed = T
)

output

library(xtable)
print.xtable(xtable(output$ANOVA), table.placement = '!h')
print.xtable(xtable(output$`Sphericity Corrections`), table.placement = '!h')

source("/Users/nathaliedusart/Desktop/RPEP/experiment/code/analysis/Pairwise_comparison.R")
start.pc <- dcast(start.cast, Participant ~ prevOut, mean, value.var = 'startRT') #long to wide
pc1 <- TES(start.pc$nonGamble, start.pc$gambleLoss, paired = TRUE)
pc2 <- TES(start.pc$nonGamble, start.pc$gambleWin, paired = TRUE)
pc3 <- TES(start.pc$gambleWin, start.pc$gambleLoss, paired = TRUE)
output <- rbind(pc1, pc2, pc3)
row.names(output) <- c('Non-Gamble vs Loss', 'Non-Gamble vs Win', 'Win vs Loss')
output

library(xtable)
print.xtable(xtable(output), table.placement = '!h')

#write data to file for overview figure
exp.start <- start.cast
save(exp.start, file = "/Users/nathaliedusart/Desktop/RPEP/experiment/data/processed/start.Rdata")

#### Latency of the choice RT ####
rt.cast <- cast(input.melt, Participant * prevOut ~ ., mean, subset = variable == "RT")
names(rt.cast)[3] <- "RT"

#plot the data
plot<- ggplot(rt.cast,aes(x=prevOut, y=RT)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.10)

plot + coord_cartesian(ylim = c(400,800)) + theme_bw() + labs (x = "Previous Outcome", 
                                                                            y = "RT (in ms)") +
  scale_x_discrete(labels = c("nonGamble" = "Non-gamble", "gambleLoss" = "Gambled Loss", "gambleWin" = "Gambled Win")) +
  theme(axis.text.x  = element_text(size=12),
             axis.text.y  = element_text(size=12),
             axis.title.x  = element_text(size=14),
             axis.title.y  = element_text(size=14)) 


summaryBy(RT ~ prevOut, data = as.data.frame(rt.cast), FUN = c(mean,sd))

output <- ezANOVA(
  data = rt.cast,
  dv = .(RT),
  wid = .(Participant),
  within = .(prevOut),
  detailed = T
)

output
library(xtable)
print.xtable(xtable(output$ANOVA), table.placement = '!h')
print.xtable(xtable(output$`Sphericity Corrections`), table.placement = '!h')

source("/Users/nathaliedusart/Desktop/RPEP/experiment/code/analysis/Pairwise_comparison.R")
rt.pc <- dcast(rt.cast, Participant ~ prevOut, mean, value.var = 'RT') #long to wide
pc1 <- TES(rt.pc$nonGamble, rt.pc$gambleLoss, paired = TRUE)
pc2 <- TES(rt.pc$nonGamble, rt.pc$gambleWin, paired = TRUE)
pc3 <- TES(rt.pc$gambleWin, rt.pc$gambleLoss, paired = TRUE)
output <- rbind(pc1, pc2, pc3)
row.names(output) <- c('Non-Gamble vs Loss', 'Non-Gamble vs Win', 'Win vs Loss')
output

library(xtable)
print.xtable(xtable(output), table.placement = '!h')

#write data to file fo overview figure
exp.rt <- rt.cast
save(exp.rt, file = "/Users/nathaliedusart/Desktop/RPEP/experiment/data/processed/RT.Rdata")

#### Analysis of start RT depending on the valence  ####
inputsub.melt <- filter(inputsub.melt, prevOut == 'gambleWin' | prevOut == 'gambleLoss')
startValence.cast <- cast(inputsub.melt, Participant * valence ~ ., mean, subset = variable == "startRT")
names(startValence.cast)[3] <- "startRT"

# plot the interaction 
plot <- ggplot(startValence.cast,aes(x=valence, y=startRT)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.10)

plot + theme(axis.text.x  = element_text(size=12),
             axis.text.y  = element_text(size=12),
             axis.title.x  = element_text(size=14),
             axis.title.y  = element_text(size=14)) + coord_cartesian(ylim = c(0,600)) + theme_bw()

summaryBy(startRT ~ valence, data=as.data.frame(startValence.cast),  FUN=c(mean,sd))

output <- ezANOVA(
  data = startValence.cast
  , dv = .(startRT)
  , wid = .(Participant)
  , within = .(valence)
  , detailed = T
)

output

source("/Users/nathaliedusart/Desktop/RPEP/experiment/Code/Analysis/Pairwise_comparison.R")
startValence.pc <- dcast(startValence.cast, Participant ~ valence,  mean, value.var = 'startRT') # long to wide
pc <- TES(startValence.pc$'Negative valence', startValence.pc$'Positive valence', paired = TRUE)
output <- rbind(pc)
row.names(output) <- c('Negative valence vs Positive valence')
output

#### Analysis of RT depending on the valence  ####
rtValence.cast <- cast(inputsub.melt, Participant * valence ~ ., mean, subset = variable == "RT")
names(rtValence.cast)[3] <- "RT"

# plot the interaction 
plot <- ggplot(rtValence.cast,aes(x=valence, y=RT)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.10)

plot + theme(axis.text.x  = element_text(size=12),
             axis.text.y  = element_text(size=12),
             axis.title.x  = element_text(size=14),
             axis.title.y  = element_text(size=14)) + coord_cartesian(ylim = c(400,900)) + theme_bw()

summaryBy(RT ~ valence, data=as.data.frame(rtValence.cast),  FUN=c(mean,sd))

output <- ezANOVA(
  data = rtValence.cast
  , dv = .(RT)
  , wid = .(Participant)
  , within = .(valence)
  , detailed = T
)

output

source("/Users/nathaliedusart/Desktop/RPEP/experiment/Code/Analysis/Pairwise_comparison.R")
rtValence.pc <- dcast(rtValence.cast, Participant ~ valence,  mean, value.var = 'RT') # long to wide
pc <- TES(rtValence.pc$'Negative valence', rtValence.pc$'Positive valence', paired = TRUE)
output <- rbind(pc)
row.names(output) <- c('Negative valence vs Positive valence')
output


#### start RT depending on valence and outcome ####
startValenceOut.cast <- cast (inputsub.melt, Participant*valence*prevOut ~ ., mean, subset = variable == 'startRT')
names(startValenceOut.cast)[4] <- 'startRT'

plot <- ggplot(startValenceOut.cast,aes(x=prevOut, y=startRT, color = valence)) + 
  stat_summary(fun = mean, geom = 'point', position = position_dodge(width = 0.20)) +
  stat_summary(fun = mean, geom = 'line', aes(group = valence), position = position_dodge(width = 0.20)) +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.10, position = position_dodge(width = 0.20)) # ZC: the within-subject error bars (standard error or CI) may be more informative

plot + theme(axis.text.x  = element_text(size=12),
             axis.text.y  = element_text(size=12),
             axis.title.x  = element_text(size=14),
             axis.title.y  = element_text(size=14)) + coord_cartesian(ylim = c(0,800)) + theme_bw()


summaryBy(startRT ~ valence + prevOut, data = as.data.frame(startValenceOut.cast), FUN = c(mean,sd))

output <- ezANOVA(
  data = startValenceOut.cast
  , dv = .(startRT)
  , wid = .(Participant)
  , within = .(valence, prevOut)
  , detailed = T
)

output


#### analysis of valence if the manipulation worked ####
negative <- filter(input, Feelings == 'Unhappy' & valence == 'Negative valence')
negative2 <- filter(input, Feelings == 'Somewhat unhappy' & valence == 'Negative valence')
positive <- filter(input, Feelings == 'Happy' & valence == 'Positive valence')
positive2 <- filter(input, Feelings == 'Somewhat happy' & valence == 'Positive valence')

test <- rbind(negative, negative2, positive, positive2)
table(test$Participant,test$Feelings) #To look at participants who are indeed influenced by the different conditions

p1 <- filter(test, Participant == 10)
p2 <- filter(test, Participant == 22)
p3 <- filter(test, Participant == 47)
p4 <- filter(test, Participant == 56)
p5 <- filter(test, Participant == 69)
p6 <- filter(test, Participant == 76)
p7 <- filter(test, Participant == 89)
p8 <- filter(test, Participant == 92)

influenced_participants <- rbind(p1,p2,p3,p4,p5,p6,p7,p8)
influenced_participants <- filter(influenced_participants, prevOut == 'gambleWin' | prevOut == 'gambleLoss')


test.melt <- melt(influenced_participants, 
                      id.var = c('Participant', 'prevOut', 'valence'), 
                      measure.var = c('startRT', 'RT'))

#### start RT & valence - influenced participants ####
startValence.cast <- cast(test.melt, Participant * valence ~ ., mean, subset = variable == "startRT")
names(startValence.cast)[3] <- "startRT"

# plot the interaction 
plot <- ggplot(startValence.cast,aes(x=valence, y=startRT)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.10)

plot + theme(axis.text.x  = element_text(size=12),
             axis.text.y  = element_text(size=12),
             axis.title.x  = element_text(size=14),
             axis.title.y  = element_text(size=14)) + coord_cartesian(ylim = c(0,600)) + theme_bw()

summaryBy(startRT ~ valence, data=as.data.frame(startValence.cast),  FUN=c(mean,sd))

output <- ezANOVA(
  data = startValence.cast
  , dv = .(startRT)
  , wid = .(Participant)
  , within = .(valence)
  , detailed = T
)

output

source("/Users/nathaliedusart/Desktop/RPEP/experiment/Code/Analysis/Pairwise_comparison.R")
startValence.pc <- dcast(startValence.cast, Participant ~ valence,  mean, value.var = 'startRT') # long to wide
pc <- TES(startValence.pc$'Negative valence', startValence.pc$'Positive valence', paired = TRUE)
output <- rbind(pc)
row.names(output) <- c('Negative valence vs Positive valence')
output

#### start RT depending on valence and outcome with influenced participants ####
startValenceOut.cast <- cast (test.melt, Participant*valence*prevOut ~ ., mean, subset = variable == 'startRT')
names(startValenceOut.cast)[4] <- 'startRT'

plot <- ggplot(startValenceOut.cast,aes(x=prevOut, y=startRT, color = valence)) + 
  stat_summary(fun = mean, geom = 'point', position = position_dodge(width = 0.20)) +
  stat_summary(fun = mean, geom = 'line', aes(group = valence), position = position_dodge(width = 0.20)) +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.10, position = position_dodge(width = 0.20)) # ZC: the within-subject error bars (standard error or CI) may be more informative

plot + theme(axis.text.x  = element_text(size=12),
             axis.text.y  = element_text(size=12),
             axis.title.x  = element_text(size=14),
             axis.title.y  = element_text(size=14)) + coord_cartesian(ylim = c(0,800)) + theme_bw()


summaryBy(startRT ~ valence + prevOut, data = as.data.frame(startValenceOut.cast), FUN = c(mean,sd))

output <- ezANOVA(
  data = startValenceOut.cast
  , dv = .(startRT)
  , wid = .(Participant)
  , within = .(valence, prevOut)
  , detailed = T
)

output

source("/Users/nathaliedusart/Desktop/RPEP/experiment/Code/Analysis/Pairwise_comparison.R")
startValenceOut.pc <- dcast(startValenceOut.cast, Participant ~ valence, mean, value.var = 'startRT') #long to wide
pc <- TES(startValenceOut.pc$'Negative valence', startValenceOut.pc$'Positive valence', paired = TRUE)
output <- rbind(pc)
row.names(output) <- c('Negative vs Positive')
output

