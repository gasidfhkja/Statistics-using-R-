work_dir <- "C:/NIL KAMAL/Teaching/2022/Design and Analysis of Experiments/R codes"
setwd(work_dir)
data.d=read.csv("Data1(Drug Consumption).CSV", header=T,stringsAsFactors=T)
## The command "stringsAsFactors" is used to transfer a string/character to factor
data.d####Data
#str(data.drug)
levels(data.d$Drug)##Levels of the data appear in alphabetical order
c.matrix=cbind(c(0.2887, 0.2887, 0.2887, -0.866),
              c(-0.7071,0.7071, 0, 0),c(-0.4082, -0.4082, 0.8165, 0))
c.matrix #Contrast matrix
contrasts(data.d$Drug) <- c.matrix##Assigned the contrast matrix to the correct levels
drug.aov=aov(Improvement~Drug, data.d)## Ordinary ANOVA
summary(drug.aov)###Summary of the Ordinary ANOVA
drug.aov$contrasts ##Verify whether the contrast matrix is assigned to the correct levels or not
#### Summary of the Augmented ANOVA ###################
summary.aov(drug.aov, split=list(Drug=list("P vs. P' "=1, "A1 vs. A2"=2, "A vs. Non-A"=3)))
###########
