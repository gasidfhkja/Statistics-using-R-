setwd("F:/Dae new")
data.Jet=read.csv("Data3(Jet).CSV", header=T,stringsAsFactors=T)
data.Jet
str(data.Jet)
###Converting the variable "Jet_Velocity" from numeric to factor
data.Jet[,"Jet_Velocity"]=factor(data.Jet[,"Jet_Velocity"])
str(data.Jet)
Jet.aov=aov(Shape~Nozzle_Design+Jet_Velocity,data=data.Jet)
summary(Jet.aov)##ANOVA aanalysis of RCBD
Jet1.aov=aov(Shape~Nozzle_Design,data=data.Jet)
summary(Jet1.aov)##ANOVA analysis of CRD (One way-classification)
#############################
########## Fisher's LSD Test %%%%%%%%%%%%%%%%%%
library(agricolae)
lsd.test =LSD.test(Jet.aov,"Nozzle_Design", alpha=0.01, group=T)
lsd.test
tukey <- TukeyHSD(Jet.aov, ordered=T, conf.level=0.99)
tukey
