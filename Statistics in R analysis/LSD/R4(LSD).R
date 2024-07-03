setwd("F:/Dae new")
###########################################################################
############ Latin Square Design  #########################################
###########################################################################
data.TV1=read.csv("Data4(TV).CSV", header=T,stringsAsFactors=T)
data.TV1
str(data.TV1)
###Converting the variables "Order" and "Operator" from numeric to factor
data.TV1[,"Order"]=factor(data.TV1[,"Order"])
data.TV1[,"Operator"]=factor(data.TV1[,"Operator"])
str(data.TV1)#To view inner-structure of the data
TV1.aov=aov(Time~Order+Operator+Method,data=data.TV1)
summary(TV1.aov)##ANOVA analysis of LSD
###############################################################
qqnorm(resid(TV1.aov))
qqline(resid(TV1.aov), col='red')## Q-Q Plot
################################ Residual Analysis ###################################
op <- par(mfrow=c(1, 2))
plot(resid(TV1.aov), main='Residual vs. Run Order')
plot(x=predict(TV1.aov), y=resid(TV1.aov), main='Predicted vs. Residual')
##########################  Tukey's Test
tukey <- TukeyHSD(TV1.aov, "Method", ordered=T, conf.level=0.99)##|\bar y_{1.}-\bar y_{3.}|
tukey
### Alternative comment for Tukey's test  #########
library(agricolae)
hsd <- HSD.test(TV1.aov, "Method", group=TRUE,alpha = 0.01)
hsd
########## Fisher's LSD Test %%%%%%%%%%%%%%%%%%
library(agricolae)
df= df.residual(TV1.aov)
df##d.f. of Error
MSerror=deviance(TV1.aov)/df
MSerror##MS_error
lsd.test =LSD.test(data.TV1$Time, data.TV1$Method, df, MSerror,alpha = 0.01)
lsd.test
###################
###############################  Dunnett's test  #######################
library(DescTools)
g=factor(data.TV1$Method)
dunnett= DunnettTest(data.TV1$Time,g, control="B", conf.level=0.99)
dunnett
###################  Scheffe's TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c.matrix=cbind( c(1/3,2/3,-1,0),c(1/5,2/5,2/5,-1))##Contrast matrix
c.matrix##Display of contrast matrix
levels(data.TV1$Method)
library(DescTools)
ScheffeTest(TV1.aov, which="Method",
            contrasts=NULL, conf.level=0.99)##For all pairwise comparisons
ScheffeTest(TV1.aov, which="Method",
            contrasts=c.matrix, conf.level=0.99)##Scheffe test for the contrast=c.matrix
#################  Multiple Comparisons using Orthogonal Contrast  ################### 
levels(data.TV1$Method)##Levels of the data appear in alphabetical order
c.matrix=cbind(c(0.2887, 0.2887, 0.2887, -0.866),
               c(-0.7071,0.7071, 0, 0),c(-0.4082, -0.4082, 0.8165, 0))
c.matrix #Contrast matrix
contrasts(data.TV1$Method) <- c.matrix##Assigned the contrast matrix to the correct levels
contrasts(data.TV1$Method) 
#TV1.aov$contrasts ##Verify whether the contrast matrix is assigned to the correct levels or not
#### Summary of the Augmented ANOVA ###################
summary.aov(TV1.aov, split=list(Method=list(" Contrast 1 "=1, "Contrast 2"=2, "Contrast"=3)))
###############################################################################
#############################  END  ###########################################
###############################################################################

###########################################################################
############ Graeco-Latin-Square Design  #########################################
###########################################################################
###########################################################################
data.TV2=read.csv("Data5(TV).CSV", header=T,stringsAsFactors=T)
data.TV2
str(data.TV2)
###Converting the variables "Order" and "Operator" from numeric to factor
data.TV2[,"Order"]=factor(data.TV2[,"Order"])
data.TV2[,"Operator"]=factor(data.TV2[,"Operator"])
str(data.TV2)#To view inner-structure of the data
TV2.aov=aov(Time~Order+Operator+Method+workplace,data=data.TV2)
summary(TV2.aov)##ANOVA analysis of LSD
###################  Scheffe's TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c.matrix=cbind( c(1/3,2/3,-1,0),c(1/5,2/5,2/5,-1))##Contrast matrix
c.matrix##Display of contrast matrix
########################### For Method #######################
levels(data.TV2$Method)
library(DescTools)
ScheffeTest(TV2.aov, which="Method",
            contrasts=NULL, conf.level=0.99)##For all pairwise comparisons
ScheffeTest(TV1.aov, which="Method",
            contrasts=c.matrix, conf.level=0.99)##Scheffe test for the contrast=c.matrix
############################# For workplace #######################
levels(data.TV2$workplace)
ScheffeTest(TV2.aov, which="workplace",
            contrasts=NULL, conf.level=0.99)##For all pairwise comparisons
ScheffeTest(TV2.aov, which="workplace",
            contrasts=c.matrix, conf.level=0.99)##Scheffe test for the contrast=c.matrix

