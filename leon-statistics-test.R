############################
##Q1
setwd("~/Downloads")   #setting directory
##Q2
library(readr)
Data_A3FF <- read_csv("Data_A3FF.csv")   #importing csv data
##Q3
View(Data_A3FF)  #view data
#################################
is.numeric(Data_A3FF$Tuition)
is.numeric(Data_A3FF$distanceKM_resid)
is.numeric(Data_A3FF$Exam_pass_rate)
######################################
##Q4
shapiro.test(Data_A3FF$Tuition)
shapiro.test(Data_A3FF$Exam_pass_rate)
shapiro.test(Data_A3FF$distanceKM_resid)
## I need shapiro test. These are all numeric. Even if have not checked their distribution if normally or not.
ks.test(Data_A3FF$Tuition,Data_A3FF$distanceKM_resid)  #check normality if numeric is normally distributed.
summary(Data_A3FF$Tuition)
summary(Data_A3FF$distanceKM_resid)
summary(Data_A3FF$Exam_pass_rate)
##############################################
##Q5
shapiro.test(Data_A3FF$Tuition)  #p-value < 0.5, it is normally distributed
shapiro.test(Data_A3FF$distanceKM_resid)   #p-value < 0.5, it is normally distributed
shapiro.test(Data_A3FF$Exam_pass_rate)    #p-value < 0.5, it is normal distributed
##################################
par(mfrow=c(1,3))
t <- density(Data_A3FF$Tuition, na.rm=T) #normally density
d <- density(Data_A3FF$distanceKM_resid, na.rm=T)  #normally density
e <- density(Data_A3FF$Exam_pass_rate, na.rm=T) # normally density
plot(t)
plot(d)
plot(e)
################################################
t.test(Data_A3FF$Tuition, mu=9817) #don't reject null. Therefore accept that they are normally
t.test(Data_A3FF$distanceKM_resid, mu=2.295)  #don't eject null. Therefore accept that they are normally
t.test(Data_A3FF$Exam_pass_rate, mu=57.05) #don't reject null. Therefore accept that they are normally
#####################################################
##Q6
# From doing shapiro test, plotting density, and doing t-test, it is reasonable to conclude they are all Normally distributed.
#########################
##Q7
cor(Data_A3FF$Tuition,Data_A3FF$distanceKM_resid, method="kendall") #both numeric and both normally
#############################################
##Q8
# there is no correlation between tuition fees and residental areas. The correlation value is negative.
####################################################################
is.numeric(Data_A3FF$Privat_public)
Data_A3FF$Privat_public<-as.factor(Data_A3FF$Privat_public)
is.factor(Data_A3FF$Privat_public)
######################################################
#9
Data_A3FF$Privat_public <- factor(Data_A3FF$Privat_public,
                                      levels = c(0,1),
                                      labels = c("private","public"))
pie(table(Data_A3FF$Privat_public))  # public not equal to private, they different. 

###########################################
##9.1
private_tution<-Data_A3FF[(Data_A3FF$Privat_public=='private'),]
public_tution<-Data_A3FF[(Data_A3FF$Privat_public=='public'),]
t.test(Tuition ~ private_tution, Data_A3FF, var.equal=TRUE)
t.test(Tuition ~ public_tution, Data_A3FF, var.equal=TRUE)
######################################
#9.2
t.test(Tuition ~ Privat_public, Data_A3FF, var.equal=TRUE)  #Numerical is normally distributed and categorical with 2 categories

