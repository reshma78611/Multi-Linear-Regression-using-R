library(readr)
startups<-read.csv('C:/Users/HP/Desktop/assignments submission/multi linear regression/50_Startups.csv')
View(startups)
start_up<-startups[,-4]
View(start_up)
attach(start_up)


###################EDA################
library(car)
pairs(start_up)
cor(start_up)

#############Model Building###############
start_model<-lm('Profit~.',data=start_up)
summary(start_model)
#R_sq=0.95
#administration and marketing spend are not significant
################validation techniques###############
car::vif(start_model)
#vif <20 so no collinearity
library(MASS)
stepAIC(start_model)
#best model=R D spend+Marketing spend

###############validation plots##########
plot(start_model)
residualPlots(start_model)
qqPlot(start_model)

##############deletion diagnostic##################
influenceIndexPlot(start_model)
#50

############Iteration 1############
start_up1<-start_up[-50,]
View(start_up1)
start_model1<-lm('Profit~.',data=start_up1)
#R_sq=0.9613 and administration and marketing spend are not significant
summary(start_model1)
#vif(start_model1)
#stepAIC(start_model1)
#plot(start_model1)
residualPlots(start_model1)
qqPlot(start_model1)
influenceIndexPlot(start_model1)
#49
##########iteration 2################
#start_up['RD_spend_sq']=R.D.Spend*R.D.Spend
#start_up['admin_sq']=Administration*Administration
start_up1<-start_up[-50,]
#View(start_up1)
start_model1<-lm('Profit~.',data=start_up1[,-2])
#R_sq=0.9611,RD spend and marketing spend are significant
summary(start_model1)
#vif(start_model1)
#stepAIC(start_model1)
#plot(start_model1)
residualPlots(start_model1)
qqPlot(start_model1)
influenceIndexPlot(start_model1)


