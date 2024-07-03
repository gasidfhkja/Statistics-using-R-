#work_dir <- "C:/NIL KAMAL/Teaching/2022/Design and Analysis of Experiments/R codes"
#setwd(work_dir)
setwd("/Users/Local Drive/Teaching/2023/DAE/R codes")
data.br=read.csv("Data2(Broker).CSV", header=T,stringsAsFactors=T)
data.br  
levels(data.br$Broker)
str(data.br)#Internal Structure of the data
boxplot(Share_Price ~ Broker, data=data.br)##Box plot of the data
br.aov=aov(Share_Price ~ Broker, data=data.br)##ANOVA##H_0:\tau_1=\tau_2=\dots=\tau_5=0
summary(br.aov)##Summary of the ANOVA
########## Fisher's LSD Test %%%%%%%%%%%%%%%%%%
library(agricolae)
lsd.test =LSD.test(br.aov,"Broker", alpha=0.01, group=T)
lsd.test
################  Tukey's HSD TEST ########################
hsd <- HSD.test(br.aov, "Broker", group=T)
hsd
### Alternative comment for Tukey's test  #########
tukey <- TukeyHSD(br.aov, ordered=T, conf.level=0.99)
tukey
###############################  Dunnett's test  #######################
library(DescTools)
g=factor(rep(1:5, c(6,6,6,6,6)),labels=c("B1", "B2", "B3",  "B4",  "B5"))
g
dunnett= DunnettTest(data.br$Share_Price,g, control="B2")
dunnett
###################  Scheffe's TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c.matrix=cbind( c(1/3,2/3,-1,0,0),c(1/5,2/5,2/5,-1,0))##Contrast matrix
c.matrix##Display of contrast matrix
c1.matrix=cbind( c(1/6,2/6,-1,2/6,1/6),c(1/5,2/5,2/5,-1/2,-1/2))
c1.matrix
library(DescTools)
ScheffeTest(br.aov, which="Broker",
            contrasts=NULL, conf.level=0.99)##For all pairwise comparisons
ScheffeTest(br.aov, which="Broker",
            contrasts=c.matrix, conf.level=0.95)##Scheffe test for the contrast=c.matrix
ScheffeTest(br.aov, which="Broker",
            contrasts=c1.matrix, conf.level=0.99)##Scheffe test for the contrast=c1.matrix
################  Model Adequacy Checking  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
qqnorm(resid(br.aov))
qqline(resid(br.aov), col='red')## Q-Q Plot
################################ Residual Analysis ###################################
#op <- par(mfrow=c(1, 2))
#dev.off()
plot(resid(br.aov), main='Residual vs. Run Order')
plot(x=predict(br.aov), y=resid(br.aov), main='Predicted vs. Residual')
########################  Equality of variance Test ##########################
bartlett.test(Share_Price ~ Broker, data=data.br) ##\epsilon_ij~N(0,\sigma^2)
###########
