## TERM PAPER
## Load Ecdat, sandwich, car and lmtest 
install.packages("Ecdat")
library(Ecdat)
install.packages("lmtest", repos="http://cran.rstudio.com/")
install.packages("sandwich", repos="http://cran.rstudio.com/")
install.packages("car")
library(lmtest)
library(sandwich)
library(car)
## Load Wages
data(Wages)
Wages
## Attach data
attach(Wages)
## Create a category variable of education and divide into 
## three different levels
edlevel <- cut(ed, c(9,12, max(ed), include.lowest=TRUE))
edlevel <- factor(edlevel, labels=c("Basic","High school","Uni"))
## Create simple regression where Y=logwage, X1=education
## named simpreg
simpreg <- lm(lwage ~ edlevel, data=Wages)
## Create regression where Y=wage, X1=education, X2=female,
## X3=experience named multireg
multireg <- lm(lwage ~ edlevel + I(ed^2) + sex + exp + (sex * union) + union, data = Wages)
## Create simple regression without levels in education
simpleed <- lm(lwage ~ ed, data=Wages)
## Create multiple regression without levels in education
multied <- lm(lwage ~ ed + I(ed^2) + sex + exp + (sex * union) + union, data = Wages)
## Summarize for estimates 
summary(simpreg)
summary(multireg)
summary(simpleed)
summary(multied)
## Get coefficient estimates
coefficients(simpreg)
coefficients(multireg)
## Run coefficient t-test to correct for robust errors
coeftest(simpreg)
coeftest(multireg)
coeftest(lm(multireg), vcov=(vcovHC(lm(multireg), "HC0")))
coeftest(lm(simpreg), vcov=(vcovHC(lm(simpreg), "HC0")))
coeftest(lm(multied), vcov=(vcovHC(lm(multied), "HC0")))
coeftest(lm(simpleed), vcov=(vcovHC(lm(simpleed), "HC0")))
## Get standard errors
coef(summary(simpreg))[,2]
coef(summary(multireg))[,2]
## Get t-values
coef(summary(simpreg))[,3]
coef(summary(multireg))[,3]
## Get the variance-covariance matrix
vcov(simpreg)
vcov(multireg)
## Get residual degrees of freedom
df.residual(simpreg)
df.residual(multireg)
## Get the residual sum of squares
deviance(simpreg)
deviance(multireg)
## Create simple and multiple regression without levels
simple <- lm(lwage ~ ed, data=Wages)
multiple <- lm(lwage ~ ed + I(ed^2) + sex + exp + (sex * union) + union, data = Wages)
## Confidence interval for simple regression
confint(simple, "ed", level = 0.95)
## Confidence interval for multiple regression
confint(multiple, "ed", level=0.95)
## F-test for simple regression
model.r <- lm(lwage ~ ed, data= Wages)
## F-test for multiple regression
model.ur <- lm(lwage ~ ed + I(ed^2) + sex + exp + (sex*union) +
union, data=Wages)
## Combine simple and multiple regression
anova(model.r, model.ur)
## F-test while correcting for heteroscedastic errors
linearHypothesis(model.ur,names(coef(model.ur))[2.5],white.adjust=TRUE)
