#install.packages("tableone")
#install.packages("Matching")
#install.packages("MatchIt")

library(tableone)
library(Matching)
library(MatchIt)

data(lalonde)

#Question 1:
#Find the standardized differences for all of the confounding 
#variables (pre-matching). What is the standardized difference 
#for married (to nearest hundredth)?

print(CreateTableOne(vars="married",strata="treat",data=lalonde,
                     test=FALSE),smd=TRUE)
#Answer: 0.719


#Question 2:
#What is the raw (unadjusted) mean of real earnings in 1978 
#for treated subjects minus the mean of real earnings in 1978 
#for untreated subjects?
mean(lalonde[lalonde$treat==1,"re78"])-mean(lalonde[lalonde$treat==0,"re78"])
#Answer: -635.0262


#Question 3:
#Fit a propensity score model. Use a logistic regression model, 
#where the outcome is treatment. Include the 8 confounding 
#variables in the model as predictors, with no interaction terms 
#or non-linear terms (such as squared terms). Obtain the 
#propensity score for each subject.

#What are the minimum and maximum values of the estimated 
#propensity score?

psmodel<-glm(treat~age+educ+black+hispan+married+nodegree+re74+re75,
             family=binomial(),data=lalonde)

#show coefficients etc
summary(psmodel)
#create propensity score
pscore<-psmodel$fitted.values

min(pscore)
max(pscore)

#Answer: 
#Min = 0.00908193
#Max = 0.8531528



#Question 4:
#Now carry out propensity score matching using the Match function.
#Before using the Match function, first do:
#set.seed(931139)
#Setting the seed will ensure that you end up with a matched data 
#set that is the same as the one used to create the solutions.
#Use options to specify pair matching, without replacement, no caliper.
#Match on the propensity score itself, not logit of the propensity score. 
#Obtain the standardized differences for the matched data.

#What is the standardized difference for married?
set.seed(931139)
#logit <- function(p) {log(p)-log(1-p)}
#psmatch<-Match(Tr=lalonde$treat,M=1,X=logit(pscore),replace=FALSE,caliper=NULL)
psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE,
               caliper=NULL)
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]
xvars<-c("age","educ","black","hispan","married","nodegree",
        "re74","re75")

#get standardized differences
matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#Answer: 0.027


#Question 5"
#For the propensity score matched data:
#Which variable has the largest standardized difference?

#Answer: black

#Question 6:
#Re-do the matching, but use a caliper this time. Set the caliper=0.1 in the options in the Match function.
#Again, before running the Match function, set the seed:
#set.seed(931139)

#How many matched pairs are there?
set.seed(931139)
psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE,
               caliper=0.1)
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]
dim(matched)

#Answer: 222 / 2 = 111


#Question 7:
#Use the matched data set (from propensity score matching with 
#caliper=0.1) to carry out the outcome analysis.

#For the matched data, what is the mean of real earnings in 1978 
#for treated subjects minus the mean of real earnings in 1978 for 
#untreated subjects?
mean(matched[matched$treat==1,"re78"]) - mean(matched[matched$treat==0,"re78"])
#Answer: 1246.806



#Question 8:
#Use the matched data set (from propensity score matching 
#with caliper=0.1) to carry out the outcome analysis.

#Carry out a paired t-test for the effect of treatment on 
#earnings. What are the values of the 95% confidence interval?
#outcome analysis
y_trt<-matched$re78[matched$treat==1]
y_con<-matched$re78[matched$treat==0]

#pairwise difference
diffy<-y_trt-y_con

#paired t-test
t.test(diffy)

#Answer: -420.0273 to 2913.6398



