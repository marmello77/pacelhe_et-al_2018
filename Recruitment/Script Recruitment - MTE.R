

data<-read.table("Recruitment_MTE.txt",h=T)
attach(data)
summary(data)


library(lme4)
library(MuMIn)

# Checking whether there is a difference in ants recruitment between treatments


mn=lmer(M.abund~1+(1|plant),REML=FALSE)
m1=lmer(M.abund~treatment+(1|plant),REML=FALSE)

anova(mn,m1)

# Significant (p = 6.776e-07). The null model can be discarded.

anova(m1)
summary(m1)

#Which treatment differs from which?

plot(M.abund~treatment)

#join the variables

library(car)

levels(treatment)

#join the variables water and amino which are the two closest in the mean.

treat.1=recode(treatment,"c('water','amino')='wateramino'")
levels(treat.1)

#Refazendo análises

m1=lmer(M.abund~treatment+(1|plant),REML=FALSE)
m2=lmer(M.abund~treat.1+(1|plant),REML=FALSE)

anova(m1,m2)

# They are different and can not group water with amino ... p = 0.0233 

#join the variables amino and sucrose 

treat.2=recode(treatment,"c('amino','sucrose')='aminosucrose'")
levels(treat.2)

#Refazendo análises

m1=lmer(M.abund~treatment+(1|plant),REML=FALSE)
m3=lmer(M.abund~treat.2+(1|plant),REML=FALSE)

anova(m1,m3)

# They are different and can not group amino with sucrose ... p = 0.00448

#join the variables sucrose and sucrose_amino 

treat.3=recode(treatment,"c('sucrose','sucrose_amino')='sucrosesacamino'")
levels(treat.3)

#Refazendo análises

m1=lmer(M.abund~treatment+(1|plant),REML=FALSE)
m4=lmer(M.abund~treat.3+(1|plant),REML=FALSE)

anova(m1,m4)

# They are not different and can not group sucrose with sucrose_amino ... p = 0.4224

# Therefore, water is different from amino, which are different from sucrose and sacamino

# Obtaining P-value 

m4=lmer(M.abund~treat.3+(1|plant),REML=FALSE)
mn=lmer(M.abund~1+(1|plant),REML=FALSE)

anova(m4,mn)

anova(m4)
summary(m4)



# Obtaining R²-value

r.squaredGLMM(m4)

#       R2m        R2c 
#   0.2284679 0.3372877
##########################################################


#Plot graphs to Minimum Time Encounter


#Rename treatments

treat.1<-recode(treatment,"c('water')='Water'")
levels(treat.1)

treat.2<-recode(treat.1,"c('amino')='Amino acids'")
levels(treat.2)

treat.3<-recode(treat.2,"c('sucrose')='Sucrose'")
levels(treat.3)

treat.4<-recode(treat.3,"c('sucrose_amino')='Sucrose+Amino acids'")
levels(treat.4)


#creating an object to calculate the mean of the categorical explanatory variable

mean.M.abund=sort(tapply(M.abund,treat.4,mean))
mean.M.abund
summary(mean.M.abund)

#create also the object for standard error (se) for continue variable response

sd.M.abund=sort(tapply(M.abund,treat.4,sd))
sd.M.abund
erro.M.abund=sort(tapply(M.abund,treat.4,sd)/sqrt(tapply(M.abund,treat.4,length)))
erro.M.abund

library(gplots)

#Creating a Barplot

barplot2(width = 1, space = 0.8, mean.M.abund,plot.ci=T,ci.u=mean.M.abund+erro.M.abund,ci.l=mean.M.abund-erro.M.abund,ylim=c(0,12),ylab="Recruitment (mean ± EP)",xlab="Treatments",col=c("grey50"),las=1,axis.lty = 1,cex.lab=1.2,cex.names = 1.4,cex.axis=1.1)
abline(h=0)


#legends

#Water
text(1.3,1.7, "a",cex.axis=1.2,family="serif")


#amino acids
text(3.1,5, "b",cex.axis=1.2,family="serif")

#Sucrose
text(4.9,8.6, "c",cex.axis=1.2,family="serif")

#Sucrose+amino acids
text(6.7,9.6, "c",cex.axis=1.5,family="serif")

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################


# Checking whether there is a difference for Minimum Time for Encounter between treatments


mn=lmer(MTE~1+(1|plant),REML=FALSE)
m1=lmer(MTE~treatment+(1|plant),REML=FALSE)

anova(mn,m1)

# Significant (p = 1.936e-06). The null model can be discarded

anova(m1)
summary(m1)


#Which treatment differs from which?

plot(MTE~treatment)

#join the variables

library(car)

levels(Treatment)


#join the variables water and amino  which are the two closest in the mean.

treat.1=recode(treatment,"c('water','amino')='wateramino'")
levels(treat.1)

#Remake Tests

m3=lmer(MTE~treatment+(1|plant),REML=FALSE)
m4=lmer(MTE~treat.1+(1|plant),REML=FALSE)

anova(m3,m4)

# They are different and can not group water with amino ... p = 0.04137

#join the variables amino and sucrose

treat.2=recode(treatment,"c('amino','sucrose')='aminosucrose'")
levels(treat.2)

#Remake Tests

m3=lmer(MTE~treatment+(1|plant),REML=FALSE)
m5=lmer(MTE~treat.2+(1|plant),REML=FALSE)

anova(m3,m5)

# They are not different and can group amino with sucrose ... p = 0.08255


#join the variables aminosucrose and sucrose_amino

treat.3=recode(treatment,"c('amino','sucrose','sucrose_amino')='aminosacarosesacamino'")
levels(treat.3)

#Remake test

m3=lmer(MTE~treatment+(1|plant),REML=FALSE)
m6=lmer(MTE~treat.3+(1|plant),REML=FALSE)

anova(m3,m6)

# They are different and can not group aminosucrose with sucrose_amino ... p = 0.0005788

# Therefore, water is different from amino and sucrose which are different from sucrose_amino

# Obtaining P-value 

m5=lmer(MTE~treat.2+(1|plant),REML=FALSE)
mn=lmer(MTE~1+(1|plant),REML=FALSE)

anova(m5,mn)

anova(m5)
summary(m5)


# Obtaining R²-value

r.squaredGLMM(m5)

#       R2m        R2c 
#   0.1968566 0.3234462 

#######################################################

#Plot graphs to Minimum Time Encounter


#Rename treatments

treat.1<-recode(treatment,"c('water')='Water'")
levels(treat.1)

treat.2<-recode(treat.1,"c('amino')='Amino acids'")
levels(treat.2)

treat.3<-recode(treat.2,"c('sucrose')='Sucrose'")
levels(treat.3)

treat.4<-recode(treat.3,"c('sucrose_amino')='Sucrose+Amino acids'")
levels(treat.4)


#creating an object to calculate the mean of the categorical explanatory variable

mean.MTE=sort(tapply(MTE,treat.4,mean))
mean.MTE
summary(mean.MTE)

#create also the object for standard error (se) for continue variable response

sd.MTE=sort(tapply(MTE,treat.4,sd))
sd.MTE
erro.MTE=sort(tapply(MTE,treat.4,sd)/sqrt(tapply(MTE,treat.4,length)))
erro.MTE

library(gplots)

#Creating a Barplot

barplot2(width = 1, space = 0.8, mean.MTE,plot.ci=T,ci.u=mean.MTE+erro.MTE,ci.l=mean.MTE-erro.MTE,ylim=c(0,40),ylab="Minimum Time Encounter (mean ± EP)",xlab="Treatments",col=c("grey50"),las=1,axis.lty = 1,cex.lab=1.2,cex.names = 1.4,cex.axis=1.1)
abline(h=0)


#legends


#sucrose+amino acids
text(1.3,13.5, "a",cex.axis=1.2,family="serif")


#sucrose
text(3.1,20, "b",cex.axis=1.2,family="serif")

#Amino acids
text(4.9,25.5, "b",cex.axis=1.2,family="serif")

#water

text(6.7,30, "c",cex.axis=1.5,family="serif")

detach(data)



