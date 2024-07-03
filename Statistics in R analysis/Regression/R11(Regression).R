work2_dir <- "F:/Design and Analysis of Experiments/R codes(DOE)"
setwd(work2_dir)
data.brake=read.csv("Data11(Compression).CSV", header=T,stringsAsFactors=T)
data.brake
str(data.brake)
mod=lm(BrakeHorsepower~rpm+RoadOctaneNumber+Compression, data=data.brake)
summary(mod)
anova(mod)
################################
# Point estimate of the mean response for a given x=x0
x0=c(1,1900,89,99)
co1=c(mod$coef)
co1
y0=sum(x0*co1)
y0 #point estimate y0=x0*bth
############################################
################################
#####################################
###############
rstandard(mod)  ## Standarized residual
rstudent(mod)   ## Studeentized residual
par(mfrow=c(1,1))  ##one plot in a given page
plot(rstandard(mod),ylab="Standarized residual")
par(mfrow=c(1,1))
plot(rstudent(mod),ylab="Studentized residual")
###########################
##  PRESS ############################################
x=model.matrix(mod)
PRESS_res=summary(mod)$res/(1-hat(x))
print(PRESS_res)
##############  Plotting of PRESS Residuals #########################
par(mfrow=c(1,1))
plot(PRESS_res,ylab="PRESS residual")
PRESS=sum(PRESS_res^2)
################  R^2 Prediction #######################
PRESS
SS_T= sum(anova(mod)$"Sum Sq")
 pred.r.squared = 1 - PRESS/(SS_T)
 pred.r.squared
 ########################################
 ####   Q-Q plot  ############################
 qqnorm(summary(mod)$res,ylab="Ordinary Residuals")
 qqline(summary(mod)$res)
 #############################################
 #install.packages("faraway")#One time to install the package ``faraway''#One time installation is required
 #library(faraway)
# par(mfrow=c(2,1))
# probDist <- pnorm(rstudent(mod))
# plot(ppoints(length(rstudent(mod))), sort(probDist),
#      main = "PP Plot_studentized", xlab = "Observed Probability", 
#      ylab = "Expected Probability") 
# abline(0,1)#add diagonal line
 ##################  Checking of non-constant variance ################
 par(mfrow=c(1,1))
 plot(mod$fit,mod$res,xlab="fitted", ylab="Residual")
 abline(h=0)
 
 
 
 
