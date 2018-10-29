library(tableone)
library(Matching)
library(ipw)
library(survey)
library(MatchIt)

data(lalonde)
#Fit a propensity score model. Use a logistic regression model, 
#where the outcome is treatment. Include the 8 confounding 
#variables in the model as predictors, with no interaction terms or
#non-linear terms (such as squared terms). Obtain the propensity score
#for each subject. Next, obtain the inverse probability of treatment 
#weights for each subject.


#Question 1
#What are thee minimum and maximum weights?
weightmodel<-ipwpoint(exposure= treat, family = "binomial", link ="logit",
                      denominator= ~ age+educ+black+hispan+married+
                        nodegree+re74+re75, data=lalonde)

#numeric summary of weights
summary(weightmodel$ipw.weights)


#Answer: 1.01 and 40.08


#Question2:
#Find the standardized differences for each confounder on the 
#weighted (pseudo) population. What is the standardized difference
#for nodegree?

weighteddata<-svydesign(ids = ~ 1, data =lalonde, weights = ~ weightmodel$ipw.weights)
xvars=c("age","educ","black","hispan","married","nodegree","re74","re75")
weightedtable <-svyCreateTableOne(vars = xvars,
                                  strata = "treat", 
                                  data = weighteddata, test = FALSE)
## Show table with SMD
print(weightedtable, smd = TRUE)

#Answer: 0.112


#Question 3:
#Using IPTW, find the estimate and 95% confidence interval
#for the average causal effect. This can be obtained from svyglm

mydata<-lalonde
mydata$wt<-weightmodel$ipw.weights
#fit a marginal structural model (risk difference)
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~wt,
                                                    data =mydata)))
coef(msm)
confint(msm,level=0.95)
#Answer: 224.6763 and  -1559.321, 2008.673


#Question 4:
#Now truncate the weights at the 1st and 99th percentiles. 
#This can be done with the trunc=0.01 option in svyglm.

#Using IPTW with the truncated weights, find the estimate and 
#95% confidence interval for the average causal effect


weightmodel<-ipwpoint(exposure= treat, family = "binomial", 
                      link ="logit",
                      denominator= ~ age+educ+black+hispan+married+
                        nodegree+re74+re75, data=lalonde, trunc=.01)

mydata<-lalonde
mydata$wt<-weightmodel$weights.trunc
#fit a marginal structural model (risk difference)
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~wt,
                                                data =mydata)))
coef(msm)
confint(msm,level=0.95)


#Answer: 486.9336   and -1090.6 to 2064.506






