

data=read.table("ER_AR_ESR.txt",h=T)
attach(data)
summary(data)
levels(treatment)


#Call packages

library(lme4)
library(MuMIn)


# Checking whether there is a difference for Encounter Rates between treatments

ER.mn=lmer(ER~1+(1|plant),REML=FALSE)
ER.m1=lmer(ER~treatment+(1|plant),REML=FALSE)

anova(ER.mn,ER.m1)

#Is significant (p=0.002557). The null model can be discarded

anova(ER.m1)
summary(ER.m1)


#Which treatment differs from which?

plot(ER~treatment)

#join the variables

library(car)

levels(treatment)

#join the variables sucrose and amino acids which are the two closest in the mean.

treat.1=recode(treatment,"c('sucrose','amino')='sucroseamino'")
levels(treat.1)

#remake the test

ER.m3=lmer(ER~treatment+(1|plant),REML=FALSE)
ER.m4=lmer(ER~treat.1+(1|plant),REML=FALSE)
anova(ER.m3,ER.m4)

anova(ER.m4)
summary(ER.m4)

# are not different and can be grouped sucrose and amino ... p = 0.09177

#sucrose does not differ from Amino


#join the variables water, amino and sucrose

treat.2=recode(treatment,"c('water','amino','sucrose')='wateraminosucrose'")
levels(treat.2)

#remake the test

ER.m3=lmer(ER~treatment+(1|plant),REML=FALSE)
ER.m5=lmer(ER~treat.2+(1|plant),REML=FALSE)
anova(ER.m3,ER.m5)

# are not different and can be grouped sucrose, amino and water ...p=  0.09861 

#water, amino and sucrose are the same

# sucrose and amino can be grouped with sacamino

treat.3=recode(treatment,"c('amino','sucrose','sucrose_amino')='aminosucrosesucamino'")
levels(treat.3)

#Remake test

ER.m3=lmer(ER~treatment+(1|plant),REML=FALSE)
ER.m6=lmer(ER~treat.3+(1|plant),REML=FALSE)
anova(ER.m3,ER.m6)

# can not be grouped sucrose_amino with the variables water, amino and sucrose .... p = 0.007879

# Obtaining the P-value comparing the minimum model against the null model

ER.m5=lmer(ER~treat.2+(1|plant),REML=FALSE)
ER.mn=lmer(ER~1+(1|plant),REML=FALSE)
anova(ER.m5,ER.mn)


anova(ER.m5)
summary(ER.m5)


#Obtaining the R² value 

r.squaredGLMM(ER.m5)

#       R2m        R2c 
#   0.05565107 0.15579642 

#Verifying the distribution

overdisp_fun(ER.m5)

# relationship between response variable and adjusted values
# 
plot (ER.m5, ER ~ fitted(.))

# errors distribution in each treatment

qqnorm (ER.m5, ~ resid(.)|plant)


#Plot graphs to Encounter Rate


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

mean.ER=sort(tapply(ER,treat.4,mean))
mean.ER
summary(mean.ER)

#create also the object for standard error (se) for continue variable response

sd.ER=sort(tapply(ER,treat.4,sd))
sd.ER
erro.ER=sort(tapply(ER,treat.4,sd)/sqrt(tapply(ER,treat.4,length)))
erro.ER

library(gplots)

#Creating a Barplot

barplot2(width = 1, space = 0.8, mean.ER,plot.ci=T,ci.u=mean.ER+erro.ER,ci.l=mean.ER-erro.ER,ylim=c(0,0.3),ylab="Encounter rates (mean ± EP)",xlab="Treatments",col=c("grey50"),las=1,axis.lty = 1,cex.lab=1.2,cex.names = 1.4,cex.axis=1.1)
abline(h=0)


#legends


#water
text(1.3,0.09, "a",cex.axis=1.2,family="serif")


#amino acids
text(3.1,0.11, "a",cex.axis=1.2,family="serif")

#Sucrase
text(4.9,0.17, "a",cex.axis=1.2,family="serif")

#Sucrase + aminoaicds
text(6.7,0.24, "b",cex.axis=1.2,family="serif")



######################################################################################################################


# Making the same tests for Attack Rate


AR.mn=lmer(AR~1+(1|plant),REML=FALSE)
AR.m1=lmer(AR~treatment+(1|plant),REML=FALSE)

anova(AR.mn,AR.m1)

#The null model can be discarded ... p = 0.0001508

anova(AR.m1)


#Which treatment differs from which?

plot(AR~treatment)


#join the variables

library(car)

levels(treatment)

#join the variables sucrose and water which are the two closest in the mean.

treat.1=recode(treatment,"c('sucrose','water')='sucrosewater'")
levels(treat.1)

#Refazendo análises

AR.m3=lmer(AR~treatment+(1|plant),REML=FALSE)
AR.m4=lmer(AR~treat.1+(1|plant),REML=FALSE)
anova(AR.m3,AR.m4)

# Are different and can not be grouped water and sucrose ... p = 0.01774


#water and amino are different?

#join the variables water and amino

treat.2=recode(treatment,"c('water','amino')='wateramino'")
levels(treat.2)

#Refazendo análises

AR.m3=lmer(AR~treatment+(1|plant),REML=FALSE)
AR.m5=lmer(AR~treat.2+(1|plant),REML=FALSE)
anova(AR.m3,AR.m5)

# They are different and can not be grouped water and amino .... p = 0.02514


#water and amino are different?

#join the variables Sucrose and amino

treat.3=recode(treatment,"c('sucrose','amino')='sucroseamino'")
levels(treat.3)

#Refazendo análises

AR.m3=lmer(AR~treatment+(1|plant),REML=FALSE)
AR.m6=lmer(AR~treat.3+(1|plant),REML=FALSE)
anova(AR.m3,AR.m6)

# Sucrose can be grouped with amino ... p = 0.8844


#sucroseamino and sucrose_amino are different?

treat.4=recode(treatment,"c('sucrose','amino','sucrose_amino')='sacaroseaminosacamino'")
levels(treat.4)

#Refazendo análises

AR.m3=lmer(AR~treatment+(1|plant),REML=FALSE)
AR.m7=lmer(AR~treat.4+(1|plant),REML=FALSE)
anova(AR.m3,AR.m7)

# can not group sucroseamino with  sucrose_amino... 0.01133

# Therefore, water is different from amino and sucrose which are different from sucrose_amino

# Obtaining the P-value comparing the minimum model against the null model

treat.ideal=recode(treatment,"c('sucrose','amino')='sucroseamino'")
levels(treat.ideal)

AR.mideal=lmer(AR~treat.ideal+(1|plant),REML=FALSE)
mn=lmer(AR~1+(1|plant),REML=FALSE)
anova(AR.mideal,mn)


anova(AR.mideal)
summary(AR.mideal)

anova(m3)
summary(AR.mideal)

# Obtaining the R² value 

r.squaredGLMM(AR.mideal)

#       R2m        R2c 
#   0.1004170 0.1457468 


# relationship between response variable and adjusted values
# 
plot (AR.m3, ER ~ fitted(.))

# errors distribution in each treatment

qqnorm (AR.m3, ~ resid(.)|plantas)


#Plot graphs to Encounter Rate


#Verifying the distribution

overdisp_fun(m3)

# relationship between response variable and adjusted values

plot (AR.m3, AR ~ fitted(.))

# errors distribution in each treatment

qqnorm (AR.m3, ~ resid(.)|plant)


#Plot graphs to Attack rate

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

mean.AR=sort(tapply(AR,treat.4,mean))
mean.AR
summary(mean.AR)

#create also the object for standard error (se) for continue variable response

sd.AR=sort(tapply(AR,treat.4,sd))
sd.AR
erro.AR=sort(tapply(AR,treat.4,sd)/sqrt(tapply(AR,treat.4,length)))
erro.AR

library(gplots)

#Creating a Barplot

barplot2(width = 1, space = 0.8, mean.AR,plot.ci=T,ci.u=mean.AR+erro.AR,ci.l=mean.AR-erro.AR,ylim=c(0,0.6),ylab="Attack rates (mean ± EP)",xlab="Treatment",col=c("grey50"),las=1,axis.lty = 1,cex.lab=1.2,cex.names = 1.4,cex.axis=1.1)
abline(h=0)


#legends

#Water
text(1.3,0.1, "a",cex.axis=1.2,family="serif")


#amino acids
text(3.1,0.28, "b",cex.axis=1.2,family="serif")

#Sucrose
text(4.9,0.29, "b",cex.axis=1.2,family="serif")

#Sucrose+amino acids
text(6.7,0.47, "c",cex.axis=1.2,family="serif")


######################################################################################################################

# Making the same tests for Exclusion Success Rate


ESR.mn=lmer(ESR~1+(1|plant),REML=FALSE)
ESR.m1=lmer(ESR~treatment+(1|plant),REML=FALSE)

anova(ESR.mn,ESR.m1)

#The null model can be discarded ... p = 8.593e-07

anova(ESR.m1)


#Which treatment differs from which?

plot(ESR~tratamento)

#join the variables

library(car)

levels(treatment)

#join the variables water and amino which are the two closest in the mean.

treat.1=recode(treatment,"c('amino','water')='aminowater'")
levels(treat.1)

#Remake test

ESR.m3=lmer(ESR~treatment+(1|plant),REML=FALSE)
ESR.m4=lmer(ESR~treat.1+(1|plant),REML=FALSE)
anova(ESR.m3,ESR.m4)

# They are different and can not be grouped water and amino ... p = 0.0389


#water and sucrose are different?

#join the variables water and sucrose 

treat.2=recode(treatment,"c('water','sucrose')='watersucrose'")
levels(treat.2)

#Remake test

ESR.m3=lmer(ESR~treatment+(1|plant),REML=FALSE)
ESR.m5=lmer(ESR~treat.2+(1|plant),REML=FALSE)
anova(ESR.m3,ESR.m5)

# Are different and can not be grouped water and sucrose .... p = 0.02286

#water is different from amino and sucrose

#sucrose and amino are different?

treat.3=recode(treatment,"c('sucrose','amino')='sucroseamino'")
levels(treat.3)

#Refazendo análises

ESR.m3=lmer(ESR~treatment+(1|plant),REML=FALSE)
ESR.m6=lmer(ESR~treat.3+(1|plant),REML=FALSE)
anova(ESR.m3,ESR.m6)

#Can group sucrose with amino ... p = 0.8183

#sucroseamino and sucrose_amino are different?

treat.4=recode(treatment,"c('sucrose','amino','sucrose_amino')='sucroseaminosacamino'")
levels(treat.4)

#Refazendo análises

ESR.m3=lmer(ESR~treatment+(1|plant),REML=FALSE)
ESR.m7=lmer(ESR~treat.4+(1|plant),REML=FALSE)
anova(ESR.m3,ESR.m7)

# can not be grouping sucroseamino with sucrose_amino ... 4.306e-05

# Therefore, water is different from amino and sucrose which are different from sacamino

# Obtaining the P-value comparing the minimum model against the null model

treat.idealESR=recode(treatment,"c('sucrose','amino')='sucroseamino'")
levels(trat.idealESR)

ESR.mideal=lmer(ESR~treat.idealESR+(1|plant),REML=FALSE)
ESR.mn=lmer(ESR~1+(1|plant),REML=FALSE)
anova(ESR.mideal,ESR.mn)


anova(ESR.mideal)
summary(ESR.mideal)
# Obtaining the R² value 

r.squaredGLMM(ESR.mideal)

#       R2m        R2c 
#   0.1732549   0.2243596


# relationship between response variable and adjusted values
# 
plot (ESR.m3, ESR ~ fitted(.))

# errors distribution in each treatment

qqnorm (ESR.m3, ~ resid(.)|plant)


#Plot graphs to Encounter Rate


#Verifying the distribution

overdisp_fun(m3)

# relationship between response variable and adjusted values

plot (ESR.m3, ESR ~ fitted(.))

# errors distribution in each treatment

qqnorm (ESR.m3, ~ resid(.)|plant)


#Plot graphs to Exclusion Success rate

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

mean.ESR=sort(tapply(ESR,treat.4,mean))
mean.ESR
summary(mean.ESR)

#create also the object for standard error (se) for continue variable response

sd.ESR=sort(tapply(ESR,treat.4,sd))
sd.ESR
erro.ESR=sort(tapply(ESR,treat.4,sd)/sqrt(tapply(ESR,treat.4,length)))
erro.ESR

library(gplots)

#Creating a Barplot

barplot2(width = 1, space = 0.8, mean.ESR,plot.ci=T,ci.u=mean.ESR+erro.ESR,ci.l=mean.ESR-erro.ESR,ylim=c(0,0.8),ylab="Exclusion Success rates (mean ± EP)",xlab="Treatment",col=c("grey50"),las=1,axis.lty = 1,cex.lab=1.2,cex.names = 1.4,cex.axis=1.1)
abline(h=0)


#legends


#Water
text(1.3,0.1, "a",cex.axis=1.2,family="serif")


#amino acids
text(3.1,0.28, "b",cex.axis=1.2,family="serif")

#Sucrose
text(4.9,0.29, "b",cex.axis=1.2,family="serif")

#Sucrose+aminoacids
text(6.7,0.6, "c",cex.axis=1.2,family="serif")


detach(data)

######################################################################################################################
