#Set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#Delete all previous objects
rm(list= ls())


#Get the data
data<- read.table("Survival.txt",header=T)
attach(data)
summary(data)
plot(data)
head(data)


######################################


#Does the time of observation depend on the treatment?
library(survival)
m.completo = survreg(Surv(Obs.T,T.exc.)~treatment) #Checke README.txt for metadata
m.n = survreg(Surv(Obs.T,T.exc.)~1)
anova(m.n,m.completo,test="Chisq")
#The null model was rejected -> p = 1.933149e-11


######################################


#Which treatments differ from one another?
library(car)
#Merge the variables water and amino acids 
treat.1=recode(treatment,"c('water','amino')='wateramino'")
levels(treat.1)

#Run the test again
m1= survreg(Surv(Obs.T,T.exc.)~treatment)
m2 = survreg(Surv(Obs.T,T.exc.)~treat.1)
anova(m1,m2,test="Chisq")
#Cannot merge water with amino -> p = 0.005447493

#Merge the variables amino acids and sucrose 
treat.2=recode(treatment,"c('amino','sucrose')='aminosucrose'")
levels(treat.2)

#Run the test again
m1= survreg(Surv(Obs.T,T.exc.)~treatment)
m3 = survreg(Surv(Obs.T,T.exc.)~treat.2)
anova(m1,m3,test="Chisq")
#These two variables do not differ from one another and so can be merged -> p = 0.5183989

#Merge the variables aminosucrose and sucrose_amino 
treat.3=recode(treatment,"c('amino','sucrose','sucrose_amino')='aminosucrosesacamino'")
levels(treat.3)

#Run the test again
m1= survreg(Surv(Obs.T,T.exc.)~treatment)
m4 = survreg(Surv(Obs.T,T.exc.)~treat.3)
anova(m1,m4,test="Chisq")
#Cannot merge aminosucrose with sucrose_amino -> p = 3.224872e-06

#Based on those tests, water is different from amino and sucrose, which are different from sacamino

#Estimate the P-value 
treat.2=recode(treatment,"c('amino','sucrose')='aminosucrose'")
levels(treat.2)
m3 = survreg(Surv(Obs.T,T.exc.)~treat.2)
m.n = survreg(Surv(Obs.T,T.exc.)~1)
anova(m.n,m3,test="Chisq")
#p= 4.029096e-12
anova(m3)
summary(m3)


######################################


#Survival plot with raw curves

#Rename the treatments
treat.4=recode(treatment,"c('water')='Water'")
levels(treat.4)

treat.5=recode(treat.4,"c('amino','sucrose')='Amino acids - sucrose'")
levels(treat.5)

treat.6=recode(treat.5,"c('sucrose_amino')='Sucrosese+Amino acids'")
levels(treat.6)

legend = c("Water","Amino acids or sucrose","Sucrose + Amino acids")
legend

#Run the model to be used for plotting
m.full = survfit(Surv(Obs.T,T.exc.)~treat.6)

#Survival plot with raw curves
plot(m.full,mark=c(1,4,6),las=1,col=c(1,4,6),ylab= "Proportion of termites remaining", xlab="Time (min)",bty="l")


######################################


#Survival plot with smooth curves

#Estimate alpha and mu
#Calculate the mean time for each group (log), that is, the mean time to death
#Calculate alpha
m.full = survreg(Surv(Obs.T,T.exc.)~treat.6)
summary(m.full)

Scale= 0.605

alpha <- 1/m.full$scale
alpha
#1.65404

#mu = exp(parameter obtained in the summary)
#Scale parameter for the construction of the curve of each treatment

#mu_water
mut1 = exp(4.440)
mut1
#84.77494

#mu_amino-sucrose
mut2 = exp(4.440-0.848)
mut2
#36.30662

#mu_sucrose+amino
mut3 = exp(4.440-1.551)
mut3
#17.97533

#TPM1 = water
TPM1 <- mut1*gamma(1+(1/alpha))
TPM1
#75.79214

#TPM2 = amino?cidos-sacarose
TPM2 <- mut2*gamma(1+(1/alpha))
TPM2
#32.45954

#TPM3 = sacarose+amino?cidos
TPM3 <- mut3*gamma(1+(1/alpha))
TPM3
#16.07065

#Survival plot with smooth curves
m.full = survfit(Surv(Obs.T,T.exc.)~treat.6)
plot(m.full,mark=c(1,4),las=1,col=c("white","white"),ylab= "Proportion of termites remaining (log)", xlab="Time (min)",bty="l",family="serif",cex.lab=1.2)

#Curve for water
curve(exp((-mut1^(-alpha))*(x^alpha)),from=0,to=50,xlab="",ylab="Proportion of termites remaining (log)",type="l",lty=1,col="gray70",lwd=2,add=T)

#Curve for amino-sucrose
curve(exp((-mut2^(-alpha))*(x^alpha)),from=0,to=50,xlab="",ylab="Proportion of termites remaining (log)",type="l",lty=2,col="gray40",log="y",lwd=2,add=T)

#Curve for sucrose+amino
curve(exp((-mut3^(-alpha))*(x^alpha)),from=0,to=50,xlab="",ylab="Proportion of termites remaining (log)",type="l",lty=1,col=1,log="y",lwd=2,add=T)


#Plot the mean time 
#Calculating the time where we have 50% mortality, an interpretation of the MU parameter
# log(S) = (-mu^-alpha)*(t^alpha)
# t^alpha = log(S)/((-mu^-alpha))
# t = (log(S)/((-mu^-alpha)))^(1/alpha)

#mean time for water
tmedio1 <- (log(0.5)/((-mut1^-alpha)))^(1/alpha)
tmedio1
abline(h=0.5,lty=2)
abline(v=tmedio1,lty=2,col=1)
points(tmedio1,0.5,pch=16,cex=1,col=1)

#mean time for amino_sacarose
tmedio2 <- (log(0.5)/((-mut2^-alpha)))^(1/alpha)
tmedio2
abline(h=0.5,lty=2)
abline(v=tmedio2,lty=2,col=1,ylim=c(0,0.5))
points(tmedio2,0.5,pch=15,cex=1.5,col="gray40")

#mean time for sucrose+amino?cidos
tmedio3 <- (log(0.5)/((-mut3^-alpha)))^(1/alpha)
tmedio3
abline(h=0.5,lty=2)
abline(v=tmedio3,lty=2,col=1)
points(tmedio3,0.5,pch=16,cex=1.5,col=1)

#Plot the legend
legend("bottomleft",legend=(legend),lty=c(1,2,1),pch=c(NA,15,16),pt.cex=0.8,col=c("gray70","gray40",1),lwd=2,bty="n")

detach(data)
