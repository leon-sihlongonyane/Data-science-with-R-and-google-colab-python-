#########Import data (excel sheet into R)
#######

library(readxl)
Example_Data <- read_excel("Downloads/Example_Data.xlsx", 
                           sheet = "Feuil1")
View(Example_Data)

########################################################################################################################################################
####Data cleaning and visualization - DATA CLEANING - UNIVARIATE ANALYSIS - MISSING DATA - HYPOTHESIS TESTING (ONE MEAN, PROPORTION)
#####################################################################################################################################


############### View the data

View(Example_Data)
head(Example_Data, n=20)
fix(Example_Data) #Change manually variable "Age..13" to "NoAge" and "Age..12" to "Age"
names(Example_Data)[names(Example_Data) == "Age...13"] <- "NoAge"
############### Merge data or add variable or observations

dim(Example_Data)
nrow(Example_Data)
ncol(Example_Data)

## cbind

favor_col<-c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,1,1,2)
favor_col
Example_Data<-cbind(Example_Data,favor_col)
View(Example_Data)

dim(Example_Data)

## Delete variable
names(Example_Data)

#Example_Data$..13
Example_Data$...13 <-NULL
Example_Data$...14 <-NULL
Example_Data$...15 <-NULL
View(Example_Data)

#setwd("/media/siaka/STORE N GO/AIMS2019/Class pratical")
setwd("~/Downloads")
save(Example_Data, file = "Example_Data.RData")

## Rbind


Example_Data45 <- read_excel("Example_Data45.xlsx",sheet = "Feuil1")
save(Example_Data45, file = "Example_Data45.RData")
names(Example_Data)
names(Example_Data45)

dim(Example_Data45)
favor_col <- c(1,2,3,4,1,2,3,4,1)
length(favor_col)
Example_Data45$...favor_col <-NULL
View(Example_Data45)
#favor_col<-c(2)
Example_Data45<-cbind(Example_Data45, favor_col)
View(Example_Data45)

View(Example_Data)
Example_Data<-rbind(Example_Data,Example_Data45)
View(Example_Data)

dim(Example_Data)

## Merge

Module_pre_evaluation2 <- read_excel("Module_pre_evaluation2.xlsx")
View(Module_pre_evaluation2)
save(Module_pre_evaluation2, file = "Module_pre_evaluation2.RData")

Example_Data<- merge(Example_Data,Module_pre_evaluation2,by="ID")
View(Example_Data)

dim(Example_Data)

## Subset of the data

names(Example_Data)[names(Example_Data) == "Age...12"] <- "Age"
View(Example_Data)


write.csv(Example_Data, "Example_DataF.csv")

Example_DataYoung<-Example_Data[Example_Data$Age<25,]
View(Example_DataYoung)

Example_DataYoungMales<-Example_Data[(Example_Data$Age<25 & Example_Data$Gender==1),]
View(Example_DataYoungMales)
Example_DataYoungMalesvar123<-Example_Data[(Example_Data$Age<25 & Example_Data$Gender==1),c(1,2,3)]
View(Example_DataYoungMalesvar123)
Example_DataMale<-Example_Data[Example_Data$Gender==1,]
View(Example_DataMale)
Example_DataSUB<-subset(Example_Data,select=c(1,2,4)) #data with only variable 1 and 3
View(Example_DataSUB) 
Example_DataSUB[3,2]<-NA
Example_DataSUB[3,2]<-10
Example_DataSUB[3,]<-NA #delete an observation

############### Check format of data and variables and change format of data and variables.
is.data.frame(Example_Data)
is.numeric(Example_Data$Age)
is.numeric(Example_Data$Gender)
is.factor(Example_Data$Gender)
Example_Data$Gender<-as.factor(Example_Data$Gender)
is.factor(Example_Data$Gender)
class(Example_Data$Gender)
class(Example_Data$Age)

### Recoding variable
Example_Data$age_group[Example_Data$Age<22]<-1
Example_Data$age_group[Example_Data$Age>=22 & Example_Data$Age<25]<-2
Example_Data$age_group[Example_Data$Age>=25]<-3
View(Example_Data)

save(Example_Data, file = "Example_Data.RData")


###############Numerical variables

summary(Age)
summary(Example_Data$Age)
mean(Example_Data$Age)
mean(Example_Data$Age,na.rm=TRUE)
median(Example_Data$Age,na.rm=TRUE)
is.na(Example_Data$Age)
table(is.na(Example_Data$Age))

sd(Example_Data$Age,na.rm=TRUE)
var(Example_Data$Age,na.rm=TRUE)
IQR(Example_Data$Age,na.rm=TRUE)

boxplot(Example_Data$Age)
boxplot(Example_Data$Age,Example_Data$Weight_Kg)
hist(Example_Data$Age, col="red",xlab="Age",ylab="Frequencies",main="Histogram of Age")
qqnorm(Example_Data$Age)
qqline(Example_Data$Age, col = "blue", lwd = 2)
d <- density(Example_Data$Age, na.rm=T)  #density plot or distribution
plot(d) 

par(mfrow=c(1,3)) #several graphs in one
boxplot(Example_Data$Age)
hist(Example_Data$Age, col="red",xlab="Age",ylab="Frequencies",main="Histogram of Age")
qqnorm(Example_Data$Age)
qqline(Example_Data$Age, col ="blue", lwd = 2)

shapiro.test(Example_Data$Age) #Test for normality
                               #Kolmogorov smirnov test  for normality

t.test(Example_Data$Age, mu=25)#Ho: mu=25 One sample t-test HYPOTHESIS TESTING
??t.test
t.test(Example_Data$Age, mu=25,alternative = c("less"))#Ho: mu=25 One sample t-test HYPOTHESIS TESTING

dim(Example_Data)
#attach(Example_Data) #Specify the data of interest to avoir repeating yourself with Example_Data$


###############Categorical

table(Example_Data$Country_region)
Example_Data$Country_region <- factor(Example_Data$Country_region,
                    levels = c(1,2,3,4,5,8),
                    labels = c("North Africa", "East Africa", "Southern Africa", "West Africa", "Central Africa","No region"))  
table(Example_Data$Country_region)

#Ex: Label: favor_col (1=black 2=pink 3=orange 4=white)
par(mfrow=c(1,1))
pie(table(Example_Data$Country_region))
barplot(table(Example_Data$Country_region),horiz=F,main="Country region")

Gender_Table<-table(Example_Data$Gender)
binom.test(Gender_Table, p=0.5) #exact binomial test: Only when two categories

Country_region_Table<-table(Example_Data$Country_region)
pt <- c(.4, .25, .20,.05, .1)
chisq.test(Country_region_Table, p=pt)
chisq.test(Country_region_Table) #Chisquare goodness-of-fit test: More than 2 categories allowed


########################################################################################################################################################
####Data analysis - BIVARIATE ANALYSIS - HYPOTHESIS TESTING (2 MEANS, 2 PROPORTIONS)
#####################################################################################################################################

############### Numeric (Normally distributed) and categorical

tapply(Example_Data$Age, Example_Data$Last_12_months_Sickness, mean, na.rm=TRUE)
tapply(Example_Data$Age, Example_Data$Last_12_months_Sickness, median, na.rm=T)

with(Example_Data, tapply(Age, Last_12_months_Sickness, shapiro.test))

#Independant with 2 group
t.test(Age ~ Last_12_months_Sickness, Example_Data, var.equal=TRUE) #Numerical is normally distributed and categorical with 2 categories

#Independant with 3+ group
result_aov <- aov(Age ~ Field, data=Example_Data) #One way ANOVA Numerical is normally distributed and categorical with 3 or more categories
summary(result_aov)
model.tables(result_aov, "means")
TukeyHSD(result_aov)   #Tukey HSD post-hoc test

#Dependant (Paired, within subject, repeated measures, matched samples) with with 2 group
t.test(numvar ~ group, Example_Data, paired=TRUE)   #Numerical is normally distributed and categorical with 2 categories

#Dependant (Paired, within subject, repeated measures, matched samples) with with 3+ groups

result_repAOV<-aov(y ~ grp_time + Error(ID/grp_time), data = Example_Data) #only for balanced data. Subject is your random and Time your fixed effect, and Time is nested (i.e. repeatedly measured) within Subject.
summary(result_repAOV)

library(lmerTest)
result_repAOV <- lmer(y ~ grp_time + (1|ID), data=Example_Data)
anova(result_repAOV)

############### Numeric (NOT Normally distributed) and categorical

library(FSA)
Summarize(Age ~ Gender, data = Example_Data) 




library(lattice)
histogram(~ Age | Gender, data=Example_Data, layout=c(1,3))      #  Histograms for each group columns and rows of individual plots

boxplot(Age ~ Gender,data = Example_Data ,ylab="xlab",xlab="ylab")

#Independant with 2 group
wilcox.test(Example_Data$Age~Example_Data$Gender) # independent 2-group Mann-Whitney U Test when the numerical variable is not normally distributed and the categorical got only 2 categories. 

#Independant with 3+ group
kruskal.test(Example_Data$Age~Example_Data$Country_region) # # Kruskal Wallis Test One Way Anova by Ranks where y is numeric and A is a factor with more than 2 categories

#Dependant (Paired, within subject, repeated measures, matched samples) with 2 group
wilcox.test(Example_Data$Y1, Example_Data$Y2, paired=TRUE)  # paired samples Wilcoxon test or Wilcoxon signed-rank test, Two data samples are matched if they come from repeated observations of the same subject.

#Dependant (Paired, within subject, repeated measures, matched samples) with 3+ group
friedman.test(y~A|B) # Randomized Block Design - Friedman Test where y is numerical, A is a grouping factor and B is a blocking factor 
friedman.test(Example_Data$x, Example_Data$w, Example_Data$t)

############### Categorical and categorical Independent

GenReg_Table<-table(Example_Data$Gender,Example_Data$Country_region)# Gender will be rows, Country_region will be columns
GenReg_Table # print table

margin.table(GenReg_Table, 1) # A frequencies (summed over B)
margin.table(GenReg_Table, 2) # B frequencies (summed over A)
prop.table(GenReg_Table) # cell percentages
prop.table(GenReg_Table, 1) # row percentages
prop.table(GenReg_Table, 2) # column percentages 


GenReg_Table<-table(Example_Data$Gender,Example_Data$Country_region)
chisq.test(GenReg_Table)
fisher.test(GenReg_Table) #For small sample sizes

############### Categorical and categorical NOT Independent
Bef_Aft_Table<-table()
mcnemar.test(Bef_Aft_Table)

############### Numerical and Numerical

cor.test(Example_Data$Gender$x, Example_Data$Gender$y, method="kendall")  #method is "kendall" or "spearman" for not normality and "Pearson" for normally distributed

############### Homogeneity of variance

# Test samples to see for homogeneity of variance (homoscedasticity). Many statistical tests assume that the populations are homoscedastic.
# Bartlett’s test - If the data is normally distributed, this is the best test to use. It is sensitive to data which is not non-normally distribution; it is more likely to return a “false positive” when the data is non-normal.
bartlett.test(y ~ group, data=Example_Data) #With one independent variable:
bartlett.test(y ~ interaction(z,group), data=Example_Data)  # For multiple Independent variables interaction must be used to combine all of them in one.


# Levene’s test - this is more robust to departures from normality than Bartlett’s test. It is in the car package.
library(car)
leveneTest(y~ group, data=Example_Data)
leveneTest(y ~ z*group, data=Example_Data)

# Fligner-Killeen test - this is a non-parametric test which is very robust against departures from normality.

fligner.test(y~ group, data=Example_Data)
fligner.test(y ~ interaction(z,group), data=Example_Data)  # For multiple Independent variables interaction must be used to combine all of them in one.



#For all these tests, the null hypothesis is that all populations variances are equal; the alternative hypothesis is that at least two of them differ.



########################################################################################################################################################
####Data analysis - MULTIVARIATE ANALYSIS - Linear regression and logistic regression
#####################################################################################################################################

##############Multiple Linear regression
Result_lm <- lm(y ~ x+z, data=Example_Data)  # Using the columns x and y from the data frame
summary(Result_lm)
coefficients(Result_lm) # model coefficients
confint(Result_lm, level=0.95) # CIs for model parameters
fitted(Result_lm) # predicted values
residuals(Result_lm) # residuals
anova(Result_lm) # anova table
vcov(Result_lm) # covariance matrix for model parameters
influence(Result_lm) # regression diagnostics 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(Result_lm)

library(car)
### Assessing Outliers
outlierTest(Result_lm) # Bonferonni p-value for most extreme obs
qqPlot(Result_lm, main="QQ Plot") #qq plot for studentized resid
leveragePlots(Result_lm) # leverage plots 
##### Influential Observations
av.Plots(Result_lm) # added variable plots
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(Example_Data)-length(Result_lm$coefficients)-2))
plot(Result_lm, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#### Normality of Residuals
qqPlot(Result_lm, main="QQ Plot") # qq plot for studentized resid
library(MASS)
sresid <- studres(Result_lm) # distribution of studentized residuals
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 
####Evaluate homoscedasticity: non-constant error variance test
ncvTest(Result_lm)
spreadLevelPlot(Result_lm) # plot studentized residuals vs. fitted values

### Multi-collinearity: Evaluate Collinearity
vif(Result_lm) # variance inflation factors
sqrt(vif(Result_lm)) > 2 # problem?
### Nonlinearity
crPlots(Result_lm) # component + residual plot
ceresPlots(Result_lm) # Ceres plots
### Non-independence of Errors:  Test for Autocorrelated Errors
durbinWatsonTest(fit)




###############Logistic regression
Result_logistic <- glm(y ~ x+z, data=Example_Data, family=binomial(link="logit"))
summary(Result_logistic)
exp(coef(Result_logistic))
confint(Result_logistic)
exp(cbind(OR = coef(Result_logistic), confint(Result_logistic)))
#compare two models
anova(model,model2,test = "Chisq") #With p > 0.05, this ANOVA test also corroborates the fact that the second model is better than first model. 



########################################################################################################################################################
#### Simulate  data set
#####################################################################################################################################

IDsim<-seq(1,10)
IDsim
Agesim<-rnorm(10,25,4)
Agesim
Gendersim<-rbinom(10,1,0.4)
Gendersim
Datasim<-data.frame(IDsim, Agesim,Gendersim)
Datasim
fix(DataE)


########################################################################################################################################################
#### reliability test Conbrach alpha
#####################################################################################################################################



install.packages("psych")
library(psych)

z<-data.frame(x, y)
alpha(z, keys=NULL)

