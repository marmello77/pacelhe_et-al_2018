#Set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#Delete all previous objects
rm(list= ls())


#Get the data
data<-read.table("Recruitment_MTE.txt",h=T)
attach(data)
summary(data)


#Load the packages
library(lme4)
library(MuMIn)
library(car)
library(gplots)


#Test for a  difference in ant recruitment between treatments
mn=lmer(M.abund~1+(1|plant),REML=FALSE)
m1=lmer(M.abund~treatment+(1|plant),REML=FALSE)
anova(mn,m1)
#The null model was rejected -> p = 6.776e-07)
anova(m1)
summary(m1)

#Which treatments differ from one another?
plot(M.abund~treatment)

#Merge the variables
levels(treatment)

#Merge the variables water and amino which are the two closest in the mean
treat.1=recode(treatment,"c('water','amino')='wateramino'")
levels(treat.1)

#Run the test again
m1=lmer(M.abund~treatment+(1|plant),REML=FALSE)
m2=lmer(M.abund~treat.1+(1|plant),REML=FALSE)
anova(m1,m2)
#The treatments water with amino cannot be merged -> p = 0.0233 

#Merge the variables amino and sucrose 
treat.2=recode(treatment,"c('amino','sucrose')='aminosucrose'")
levels(treat.2)

#Run the test again
m1=lmer(M.abund~treatment+(1|plant),REML=FALSE)
m3=lmer(M.abund~treat.2+(1|plant),REML=FALSE)
anova(m1,m3)
#The treatments amino with sucrose cannot be merged -> p = 0.00448

#Merge the variables sucrose and sucrose_amino 
treat.3=recode(treatment,"c('sucrose','sucrose_amino')='sucrosesacamino'")
levels(treat.3)

#Run the test again
m1=lmer(M.abund~treatment+(1|plant),REML=FALSE)
m4=lmer(M.abund~treat.3+(1|plant),REML=FALSE)
anova(m1,m4)
#The null model was rejected -> p = 0.4224

#Therefore, water is different from amino, which is different from sucrose and sacamino

#Estimate the P-value 
m4=lmer(M.abund~treat.3+(1|plant),REML=FALSE)
mn=lmer(M.abund~1+(1|plant),REML=FALSE)
anova(m4,mn)
anova(m4)
summary(m4)

#Calculate the R value
r.squaredGLMM(m4)
#       R2m        R2c 
#   0.2284679 0.3372877


#Plot the graphs for Recruitment
#Rename the treatments
treat.1<-recode(treatment,"c('water')='Water'")
levels(treat.1)

treat.2<-recode(treat.1,"c('amino')='Amino acids'")
levels(treat.2)

treat.3<-recode(treat.2,"c('sucrose')='Sucrose'")
levels(treat.3)

treat.4<-recode(treat.3,"c('sucrose_amino')='Sucrose+Amino acids'")
levels(treat.4)


#Calculate the mean of the categorical explanatory variable
mean.M.abund=sort(tapply(M.abund,treat.4,mean))
mean.M.abund
summary(mean.M.abund)

#Calculate the standard error (se) for the continuous response variable
sd.M.abund=sort(tapply(M.abund,treat.4,sd))
sd.M.abund
erro.M.abund=sort(tapply(M.abund,treat.4,sd)/sqrt(tapply(M.abund,treat.4,length)))
erro.M.abund

#Make a Barplot
barplot2(width = 1, space = 0.8, mean.M.abund,plot.ci=T,ci.u=mean.M.abund+erro.M.abund,ci.l=mean.M.abund-erro.M.abund,ylim=c(0,12),ylab="Recruitment (mean EP)",xlab="Treatments",col=c("grey50"),las=1,axis.lty = 1,cex.lab=1.2,cex.names = 1.4,cex.axis=1.1)
abline(h=0)

#Significance letters
#Water
text(1.3,1.7, "a",cex.axis=1.2,family="serif")

#amino acids
text(3.1,5, "b",cex.axis=1.2,family="serif")

#Sucrose
text(4.9,8.6, "c",cex.axis=1.2,family="serif")

#Sucrose+amino acids
text(6.7,9.6, "c",cex.axis=1.5,family="serif")


##############################


#Test for a difference in Minimum Time for Encounter between treatments
mn=lmer(MTE~1+(1|plant),REML=FALSE)
m1=lmer(MTE~treatment+(1|plant),REML=FALSE)
anova(mn,m1)
#The null model was rejected -> p = 1.936e-06
anova(m1)
summary(m1)

#Which treatments differ from one another?
plot(MTE~treatment)

#Merge the variables
levels(Treatment)

#Merge the variables water and amino, which are the two closest in the mean.
treat.1=recode(treatment,"c('water','amino')='wateramino'")
levels(treat.1)

#Run the test again
m3=lmer(MTE~treatment+(1|plant),REML=FALSE)
m4=lmer(MTE~treat.1+(1|plant),REML=FALSE)
anova(m3,m4)
#The variables cannot be merged -> p = 0.04137

#Merge the variables amino and sucrose
treat.2=recode(treatment,"c('amino','sucrose')='aminosucrose'")
levels(treat.2)

#Run the test again
m3=lmer(MTE~treatment+(1|plant),REML=FALSE)
m5=lmer(MTE~treat.2+(1|plant),REML=FALSE)
anova(m3,m5)
#The variables can be merged -> p = 0.08255

#Merge the variables aminosucrose and sucrose_amino
treat.3=recode(treatment,"c('amino','sucrose','sucrose_amino')='aminosacarosesacamino'")
levels(treat.3)

#Run the test again
m3=lmer(MTE~treatment+(1|plant),REML=FALSE)
m6=lmer(MTE~treat.3+(1|plant),REML=FALSE)
anova(m3,m6)
#The variables cannot be merged -> p = 0.0005788

#Therefore, water is different from amino and sucrose, which are different from sucrose_amino

#Estimate the P-value 
m5=lmer(MTE~treat.2+(1|plant),REML=FALSE)
mn=lmer(MTE~1+(1|plant),REML=FALSE)
anova(m5,mn)
anova(m5)
summary(m5)

#Estimate the R value
r.squaredGLMM(m5)
#       R2m        R2c 
#   0.1968566 0.3234462 


#Plot the graphs for Minimum Time Encounter

#Rename the treatments
treat.1<-recode(treatment,"c('water')='Water'")
levels(treat.1)

treat.2<-recode(treat.1,"c('amino')='Amino acids'")
levels(treat.2)

treat.3<-recode(treat.2,"c('sucrose')='Sucrose'")
levels(treat.3)

treat.4<-recode(treat.3,"c('sucrose_amino')='Sucrose+Amino acids'")
levels(treat.4)


#Calculate the mean of the categorical explanatory variable
mean.MTE=sort(tapply(MTE,treat.4,mean))
mean.MTE
summary(mean.MTE)

#Calculate the standard error (se) for the continuous reponse variable
sd.MTE=sort(tapply(MTE,treat.4,sd))
sd.MTE
erro.MTE=sort(tapply(MTE,treat.4,sd)/sqrt(tapply(MTE,treat.4,length)))
erro.MTE

#Make a Barplot
barplot2(width = 1, space = 0.8, mean.MTE,plot.ci=T,ci.u=mean.MTE+erro.MTE,ci.l=mean.MTE-erro.MTE,ylim=c(0,40),ylab="Minimum Time Encounter (mean ? EP)",xlab="Treatments",col=c("grey50"),las=1,axis.lty = 1,cex.lab=1.2,cex.names = 1.4,cex.axis=1.1)
abline(h=0)

#Significance letters
#sucrose+amino acids
text(1.3,13.5, "a",cex.axis=1.2,family="serif")

#sucrose
text(3.1,20, "b",cex.axis=1.2,family="serif")

#Amino acids
text(4.9,25.5, "b",cex.axis=1.2,family="serif")

#water

text(6.7,30, "c",cex.axis=1.5,family="serif")

detach(data)
