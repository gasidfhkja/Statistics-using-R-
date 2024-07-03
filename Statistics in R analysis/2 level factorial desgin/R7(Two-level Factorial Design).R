setwd("/Users/Local Drive/Teaching/2023/DAE/R codes")
Data.Chem=read.csv("Data10(Chem).CSV", header=T,stringsAsFactors=T)
Data.Chem ### Data
str(Data.Chem)##Internal Structure of the data
Data.av=aov(yield~A*B, data=Data.Chem)
summary(Data.av)##ANOVA analysis of 2-lvel factor Factorial Design
reg=lm(yield~A+B,data=Data.Chem)
summary(reg)
##############################################################################
##################  2^3 Factorial Design  ######################################
###############################################################################
Machine= matrix(c(21,31,25,33,43,29,34,34,50,
                  56,47,46,43,45,38,41,37,36,61,50,54,40,41,47),byrow=T,ncol=3)
dimnames(Machine) = list(c("(1)","a","b","ab","c","ac","bc","abc"),
                         c("Rep1","Rep2", "Rep3"))
A = rep(c(-1,1),4)
B =rep(c(-1,-1,1,1),2)
C= c(rep(-1,4),rep(1,4))
Total = apply(Machine,1,sum)
cbind(A,B,C,Machine,Total)##The given data
##########################################
I=c(rep(1,8))
AB = A*B
AC = A*C
BC = B*C
ABC = A*B*C
Design.matrix=cbind(I, A,B,AB,C, AC,BC,ABC,Total)
Design.matrix
##################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################### Effect estimates  ####################
#Aeff = (Total %*% A)/(4*n)
#Beff= (Total %*% B)/(4*n)
#Ceff= (Total %*% C)/(4*n)
#ABeff = (Total %*% AB)/(4*n)
#ACeff = (Total %*% AC)/(4*n)
#BCeff = (Total %*% BC)/(4*n)
#ABCeff = (Total %*% ABC)/(4*n)
#Ieff=(Total %*% I)/(8*n)
################## Interaction and Main effects  ########################
n = 3 ##Replication
Feff = t(Total) %*% cbind(A,B,AB,C,AC,BC,ABC)/(4*n)
Ieff=t(Total) %*% cbind(I)/(8*n)
eff=cbind(Ieff,Feff)
Summary = rbind( cbind(I,A,B,AB,C, AC,BC,ABC),eff )
dimnames(Summary)[[1]] = c(dimnames(Machine)[[1]],"Effect")
Summary
########################  ANOVA Model ##############
Machine1= c(t(Machine))
Af= rep(as.factor(A),rep(3,8))
Bf= rep(as.factor(B),rep(3,8))
Cf= rep(as.factor(C),rep(3,8))
data.m=data.frame(Af,Bf,Cf,Machine1)
data.m
Machine.av=aov(Machine1 ~ Af*Bf*Cf, data=data.m)
summary(Machine.av)

