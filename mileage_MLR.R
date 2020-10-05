library(readr)
mileage<-read.csv('C:/Users/HP/Desktop/datasets/Cars.csv')
View(mileage)
attach(mileage)

#########EDA############
library(car)
pairs(mileage)
# scatter plot of all data
cor(mileage)
#correlation 

##########Model building###########
model<-lm('MPG~.',data=mileage)
summary(model)
#R_sq=77.05
#we observe collinearity between  VOl,WT since p-val > 0.05, but to see clearly about collinearity and further action we go for validation techniques
#Vol and WT are insignificant


###############validation Techniques###########
#check for collinearity using VIF(Variance inflation factor)
car::vif(model)
#if vif>20 then collinearity
# here we observe  high collinearity b/w vol and wt
# now we need to build a model by excluding vol or WT as they both are collinear, but which one is decided by subset selection

#subset selection is done by AIC(Akaik Information criteria)
#install.packages('MASS')
library(MASS)
stepAIC(model)
#best subset is for which AIC is less, here we got less AIC value when WT is excluded
#best model=HP+SP+VOL

################Validation plots############
#plot(model)
#residual vs fitted plot,QQplot,standardised residual vs fitted plot,residual vs leverage
residualPlots(model)
#plots b/w residuals and HP,SP,VOL,WT
#in this we get non linear patterns for HP and SP so to correct them apply transformations
qqPlot(model)
# to check for normality and we can observe some outliers here


#######################Deletion Diagnostic#################
influenceIndexPlot(model)
#to find influencers or outliers so that we can delete them
#here 77th data point is outlier
# so remove 77th data point and also apply transformation (from line 37) and go for same process till now

################  Iteration 1,2,3  #####################
mileage['HP2']<-HP*HP
mileage['SP2']<-SP*SP
#transformations HP_sq and SP_sq
mileage1<-mileage[-c(81,77,66),]
#77th observation is deleted
#now for this data bulid a model
model1<-lm('MPG~.',data=mileage1)
summary(model1)
#R_sq=92.16,0.9298,0.958
#vol and WT are insignificant
car::vif(model1)
#HP and HP2 , SP and SP2, VOL and WT are having collinearity
#plot(model1)
residualPlots(model1)
# since we applied transformation we get linear patterns for HP and SP
qqPlot(model1)
# for normality check
influenceIndexPlot(model1)
#here we observe 79th data point as outliers, so can remove if need higher R_sq
#[VOL,WT] are having collinearity so from both vol and WT we can remove WT 


###############   Iteration 4  #############################
mileage2<-mileage[-c(77,81,66,79),]
#View(mileage2)
model2<-lm('MPG~.',data=mileage2[,-5])
summary(model2)
#R_sq=0.975 but sp and sp2 are not significant
car::vif(model2)
#VIF val for sp2 is high so del it
residualPlots(model2)

############### Final  Iteration   #############################
mileage2<-mileage[-c(77,81,66,79),]
#View(mileage2)
final_model<-lm('MPG~.',data=mileage2[,-c(5,7)])
summary(final_model)
#R_sq=0.975, all are significant 
residualPlots(final_model)



