library(readr)
toyota<-read.csv('C:/Users/HP/Desktop/assignments submission/multi linear regression/ToyotaCorolla.csv')
View(toyota)
Toyota<-toyota[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Toyota)
attach(Toyota)
sum(is.na(Toyota))
Toyota<-na.omit(Toyota)

#################EDA#############
library(car)
pairs(Toyota)
cor(Toyota)

#####################Model building##################
toyota_model<-lm('Price~.',data=Toyota)
summary(toyota_model)
#R_sq=0.863
#cc,doors are not significant

###############Validation techniques##########
car::vif(toyota_model)
library(MASS)
stepAIC(toyota_model)
##############validation plots###########
plot(toyota_model)
residualPlots(toyota_model)
qqPlot(toyota_model)
influenceIndexPlot(toyota_model)

##############Iteration 1#########
Toyota1<-Toyota[-81,]
toyota_model1<-lm('Price~.',data=Toyota1)
summary(toyota_model1)
#R_sq=0.869
#doors is not significant
residualPlots(toyota_model1)
influenceIndexPlot(toyota_model1)
#222

############Iteration 2,3,4################
Toyota1<-Toyota[-c(81,222,961,602),]
toyota_model1<-lm('Price~.',data=Toyota1)
summary(toyota_model1)
#R_sq=0.877,0.885,0.889
#all are significant
residualPlots(toyota_model1)
influenceIndexPlot(toyota_model1)
#961,602



############Transformations##########
Toyota['Door_sq']<-Doors*Doors
Toyota['cc_sq']<-cc*cc
Toyota1<-Toyota[-c(81,222,961,602),]
toyota_model2<-lm('Price~.',data=Toyota1)
summary(toyota_model2)
#R_sq=0.8926
#Quarterly _tax is insignificant
residualPlots(toyota_model2)

Toyota['Door_sq']<-Doors*Doors
Toyota1<-Toyota[-c(81,222,961,602),]
toyota_model2<-lm('Price~.',data=Toyota1)
summary(toyota_model2)
#R_sq=0.8913
#all are significant

Toyota1<-Toyota[-c(81,222,961,602),]
toyota_model2<-lm('Price~Age_08_04+KM+HP+log(cc)+log(Doors)+Gears+Quarterly_Tax+Weight+Door_sq',data=Toyota1)
summary(toyota_model2)
#R_sq=0.8917
#all are significant


Toyota1<-Toyota[-c(81,222,961,602),]
toyota_model2<-lm('Price~Age_08_04+KM+HP+cc+log(Doors)+Gears+Quarterly_Tax+Weight+Door_sq',data=Toyota1)
summary(toyota_model2)
#R_sq=0.8911
#all are significant

Toyota1<-Toyota[-c(81,222,961,602),]
toyota_model2<-lm('Price~Age_08_04+KM+HP+log(cc)+Doors+Gears+Quarterly_Tax+Weight+Door_sq',data=Toyota1)
summary(toyota_model2)
#R_sq=0.8901
#all are significant



Toyota1<-Toyota[-c(81,222,961,602),]
toyota_model2<-lm('Price~Age_08_04+KM+HP+log(cc)+Doors+Gears+Quarterly_Tax+Weight+Door_sq',data=Toyota1)
summary(toyota_model2)
#R_sq=0.8919
#all are significant








