#clear memory
rm(list=ls())

install.packages('readxl')
install.packages('lme4')
install.packages('ggcorrplot')

library(readxl)
library(lme4)
library(nlme)
library(rpart.plot)
library(rpart)	
library(readxl)
library(randomForest)
library(ggplot2)
library(caTools)
library(party)
library(forestFloor)
library(caret)
library(tree)
library(CORElearn)
library(rtf)
library(corrplot)
library(MASS)
library(ggcorrplot)


#Import data 
fingerdata <- read_xlsx("C:\\Users\\njohnson1\\Desktop\\Exp_Design_612\\FingerForce.xlsx")
fingerforce <- data.frame(fingerdata)

fingerforce$finger = as.factor(fingerforce$finger)
fingerforce$location = as.factor(fingerforce$location)
fingerforce$individual = as.factor(fingerforce$individual)
fingerforce$trial = as.factor(fingerforce$trial)

fitM0 <- lmer(force ~ finger*location + (1|individual), data = fingerforce, method = "ML") 
summary(fitM0)



#fingerfit <- lmer(force ~ finger*location, data=fingerforce, random = individual)
#summary(fingerfit)

plot(fitM0)

aov.out = aov(force ~ finger*location + (1|individual),data=fingerforce)
summary(aov.out)

install.packages('sjPlot')

library(sjPlot)
library(sjmisc)

ggplot(fitM0, aes(finger, force, fill=individual, color=individual)) + geom_violin() + theme_bw(base_size=12)
ggplot(fitM0, aes(finger, force, fill=location, color=location)) + geom_violin() + theme_bw(base_size=12) 



#model2
fitM1 <- lmer(force ~ finger*individual + (1|trial), data = fingerforce, method = "ML") 
summary(fitM1)

plot(fitM1)


aov.out1 = aov(force ~ finger*individual + trial,data=fingerforce)
summary(aov.out1)

ggplot(fitM1, aes(finger, force, fill=individual, color=individual)) + geom_violin() + theme_bw(base_size=12)
ggplot(fitM1, aes(finger, force, fill=location, color=location)) + geom_violin() + theme_bw(base_size=12)


