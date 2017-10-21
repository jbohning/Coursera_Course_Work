#Example of binary analysis with wins and losses for the Baltimore Ravens:
download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda"
              , destfile="/Users/JessicaBohning/Documents/Data Science/Coursera Docs/Course 7- Regression Modeling/ravensData.rda",method="curl")
load("/Users/JessicaBohning/Documents/Data Science/Coursera Docs/Course 7- Regression Modeling/ravensData.rda")
head(ravensData)
lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRavens)$coef #Linear is not a great model for wins and losses 

#Visualizing Fitting Logistic Regression Curves
library(manipulate)
x <- seq(-10, 10, length = 1000)
manipulate(
        plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)), 
             type = "l", lwd = 3, frame = FALSE),
        beta1 = slider(-2, 2, step = .1, initial = 2),
        beta0 = slider(-2, 2, step = .1, initial = 0)
)

#Fit the Ravens data to a good curve, now
logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,family="binomial")
summary(logRegRavens)
plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",xlab="Score",ylab="Prob Ravens Win")
exp(logRegRavens$coeff)
exp(confint(logRegRavens))
#Anova function- not so useful here, but more useful in determining the best model
anova(logRegRavens,test="Chisq")







