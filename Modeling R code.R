rm(list=ls())

library(readxl)
library(Hmisc) 

#load data
multdata <- read_excel("HW2_MultimediaHW.xlsx")
#obtain summary stats for the data

summary(multdata)
colnames(multdata)

#Generate the Variables of interest
## Dependent variable (sales)
Sales<-multdata$`Sales (units)`

## Independent variables (media types)
## Omitting aggregate variables (ADV Total, Offline, Online) because they are functions of other variables
off1<-multdata$Catalogs_ExistCust
off2<-multdata$Catalogs_Winback
off3<-multdata$Catalogs_NewCust
off4<-multdata$Mailings
on1<-multdata$Search
on2<-multdata$Banner
#on3<-multdata$SocialMedia <- column is empty
on4<-multdata$Newsletter
on5<-multdata$Retargeting
on6<-multdata$Portals

#2a)
#Diminishing Returns model - sqrt(x)
SqM1=sqrt(off1)
SqM2=sqrt(off2)
SqM3=sqrt(off3)
SqM4=sqrt(off4)
SqM5=sqrt(on1)
SqM6=sqrt(on2)
#SqM7=sqrt(on3)
SqM8=sqrt(on4)
SqM9=sqrt(on5)
SqM10=sqrt(on6)

#Running model with intercept

regmod1<-lm(Sales~SqM1+SqM2+SqM3+SqM4+SqM5+SqM6+SqM8+SqM9+SqM10)
summary(regmod1)
AIC(regmod1)
BIC(regmod1)

#2b)
#Diminishing Returns model - ln(x)

#Transform variables by taking natural log

ln1=log(off1)
ln2=log(off2)
ln3=log(off3)
ln4=log(off4)
ln5=log(on1) #No -Inf values
ln6=log(on2)
#ln7=log(on3)
ln8=log(on4) #No -Inf values
ln9=log(on5)
ln10=log(on6) #No -Inf values

#R cannot model variables containing the ln of zero, so we need to replace these with 0.

#Create a function that replaces the -Inf values of the ln-vectors with 0
replace_inf <- function(element) {
  element <- ifelse(is.infinite(element), 0, element)
  return(element)
}

ln1 <- sapply(ln1, replace_inf)
ln2 <- sapply(ln2, replace_inf)
ln3 <- sapply(ln3, replace_inf)
ln4 <- sapply(ln4, replace_inf)
ln6 <- sapply(ln6, replace_inf)
ln9 <- sapply(ln9, replace_inf)

#Run a regression model on the variables
regmod2<-lm(Sales~ln1+ln2+ln3+ln4+ln5+ln6+ln8+ln9+ln10)
summary(regmod2)
AIC(regmod2)
BIC(regmod2)

#AIC and BIC dropped in this model


#4)
#Dynamic model with lag term Y(t-1). We are using the focal model, which includes the sqrt(x) variables.

#Create the lag variable
Stm1 <- Lag(Sales,shift=1) 

regmod3 <-lm(Sales~Stm1+SqM1+SqM2+SqM3+SqM4+SqM5+SqM6+SqM8+SqM9+SqM10)
summary(regmod3)
AIC(regmod3)
BIC(regmod3)

#MODEL SELECTION PHASE
# Drop insignificant variables one at a time to minimize AIC and BIC.
# Currently no predictors are statistically significant at the 5% level of significance.

regmod4 <-lm(Sales~Stm1+SqM1+SqM2+SqM3+SqM4+SqM5+SqM8+SqM9+SqM10) #Dropped var 6
summary(regmod4)
AIC(regmod4)
BIC(regmod4)

regmod5 <-lm(Sales~Stm1+SqM1+SqM2+SqM3+SqM5+SqM8+SqM9+SqM10) #Dropped var 4
summary(regmod5)
AIC(regmod5)
BIC(regmod5)

regmod6 <-lm(Sales~Stm1+SqM1+SqM2+SqM3+SqM8+SqM9+SqM10) #Dropped var 5
summary(regmod6)
AIC(regmod6)
BIC(regmod6)
#More variables are showing significance

regmod7 <-lm(Sales~Stm1+SqM1+SqM2+SqM3+SqM8+SqM10) #Dropped var 9
summary(regmod7)
AIC(regmod7)
BIC(regmod7)

regmod8 <-lm(Sales~Stm1+SqM1+SqM2+SqM3+SqM10) #Dropped var 8
summary(regmod8)
AIC(regmod8)
BIC(regmod8)

regmod9 <-lm(Sales~Stm1+SqM2+SqM3+SqM10) #Dropped var 1
summary(regmod9)
AIC(regmod9)
BIC(regmod9)
#This is our final focal model.

finalmod <- regmod9


#6)
#Testing other models

#Dynamic model with ln transformation:
regmod10 <- lm(Sales~Stm1+ln1+ln2+ln3+ln4+ln5+ln6+ln8+ln9+ln10)
summary(regmod10)
AIC(regmod10) #lower than original ln model
BIC(regmod10) #lower than original ln model

#ln transformation with no lag:
regmod11 <- lm(Sales~ln1+ln2+ln3+ln4+ln5+ln6+ln8+ln9+ln10)
summary(regmod11)
AIC(regmod11) #increased
BIC(regmod11) #increased

#Focal model with no lag
regmod_1 <-lm(Sales~SqM2+SqM3+SqM10) #Dropped
summary(regmod_1)
AIC(regmod_1) #increased
BIC(regmod_1) #increased

#Omitting the lag term worsens the model performance.

#Focal model with no intercept
regmod12 <-lm(Sales~Stm1+SqM2+SqM3+SqM10-1)
summary(regmod12)
AIC(regmod12) #increased from final focal model
BIC(regmod12) #increased from final focal model

#We will assume the model performs best with an intercept and a lag term (dynamic)
#The ln model with lag performs best. Drop variables from regmod 10 to minimize AIC/BIC.


regmod21 <- lm(Sales~Stm1+ln1+ln2+ln3+ln4+ln5+ln8+ln9+ln10)
summary(regmod21)
AIC(regmod21) #decreased
BIC(regmod21) #decreased

regmod22 <- lm(Sales~Stm1+ln1+ln2+ln3+ln5+ln8+ln9+ln10)
summary(regmod22)
AIC(regmod22) #decreased
BIC(regmod22) #decreased

regmod23 <- lm(Sales~Stm1+ln1+ln2+ln3+ln5+ln8+ln10)
summary(regmod23)
AIC(regmod23) #decreased
BIC(regmod23) #decreased

#Dynamic ln model with 5 variables plus lag term
regmod13 <- lm(Sales~Stm1+ln1+ln2+ln3+ln8+ln10)
summary(regmod13)
AIC(regmod13)
BIC(regmod13)

#Dynamic ln model with 3 variables plus lag term
regmod14 <- lm(Sales~Stm1+ln2+ln3+ln10)
summary(regmod14)
AIC(regmod14) #did not decrease
BIC(regmod14) #did not decrease

#We will keep the 5 variable model and designate this as the new best model

bestmodel <- regmod13

#Test for synergistic effects using the new best model
regmod18 <- lm(Sales~Stm1+
               ln1*ln8+ln1*ln10+
               ln2*ln8+ln2*ln10+
               ln3*ln8+ln3*ln10+
               ln8*ln10)
summary(regmod18)
AIC(regmod18) #went down
BIC(regmod18) #went up
#Only ln8*ln2 are significant

regmod19 <- lm(Sales~Stm1+ln1+ln2*ln8+ln3+ln10)
summary(regmod19)
AIC(regmod19) #Improved
BIC(regmod19) #Improved







