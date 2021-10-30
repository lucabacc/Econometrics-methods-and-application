####################
# Coursera TE_Week4
####################

model <- lm(logw ~ educ + exper + exper_2 + smsa + south, data = Final4)

summary(model)

# Print table with results
install.packages("stargazer")
library("stargazer")
stargazer(model,
           title="Measure of returns to schooling",
           covariate.labels= c("Years of schooling","Working experience in years","Working experience in years (quadratic)", 
                               "Metropolitan dummy", "South Dummy"),
           notes.label="Significance level",
           type="html",
           out= "Documents/Università/Magistrale/Coursera/Econometrics/Rotterdam school of management/Week 4/tables.htm")

################
# Question c)
################

test1 <- lm(exper ~ age, data = Final4)
summary(test1)

install.packages("stargazer")
library("stargazer")
stargazer(test1,
          title="Measure of correlation with working experience and age",
          covariate.labels= c("Age"),
          notes.label="Significance level",
          type="html",
          out= "Documents/Università/Magistrale/Coursera/Econometrics/Rotterdam school of management/Week 4/test1.htm")

test2 <- lm(model$residuals ~ age, data = Final4)
summary(test2)

library("stargazer")
stargazer(test2,
          title="Measure of correlation with residuals experience and age",
          covariate.labels= c("Age"),
          notes.label="Significance level",
          type="html",
          out= "Documents/Università/Magistrale/Coursera/Econometrics/Rotterdam school of management/Week 4/test2.htm")

################
# Question D)
################

#Regress education against the possible instruments, which are: age, age^2, nearc, dadeduc and momeduc

test_d1 <- lm(educ ~ age + age_2 + smsa + south + nearc + daded + momed, data = Final4)
summary(test_d1)

library("stargazer")
stargazer(test_d1,
          title="First stage: Education as dependent variable",
          notes.label="Significance level",
          type="html",
          out= "Documents/Università/Magistrale/Coursera/Econometrics/Rotterdam school of management/Week 4/test2_d1.htm")

###############
# Question e)
###############

attach(Final4)

#Defining variables

Y1 <- cbind(logw)
Y2 <- cbind(educ, exper, exper_2) #endogenous variables
X1 <- cbind(smsa, south) #exogenous dummies
X2 <- cbind(age, age_2, nearc, daded, momed)

#2SLS estimation, fast way
library(AER)
library(sistemfit)
ivreg <- ivreg(Y1 ~ Y2 + X1 | X1 + X2)
summary(ivreg)

stargazer(ivreg,
          title="2SLS",
          notes.label="Significance level",
          type="html",
          out= "Documents/Università/Magistrale/Coursera/Econometrics/Rotterdam school of management/Week 4/ivreg.htm")

summary(ivreg, diagnostics = "true")





           
