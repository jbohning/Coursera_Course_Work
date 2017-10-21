n<-100
x<-rnorm(n)
x2<-rnorm(n)
x3<-rnorm(n)
y<-1+x+x2+x3+rnorm(n,sd=.1)
ey<-resid(lm(y~x2+x3))
ex<-resid(lm(x~x2+x3))
sum(ey*ex)/sum(ex^2)
coef(lm(ey~ex-1))
coef(lm(y~x+x2+x3))


#Example 2
require(datasets)
data(swiss)
require(GGally)
require(ggplot2)
# Function to return points and geom_smooth
# allow for the method to be changed
my_fn <- function(data, mapping, method="loess", ...){
        p <- ggplot(data = data, mapping = mapping) + 
                geom_point() + 
                geom_smooth(method=method, ...)
        p
}

# Default loess curve    
ggpairs(swiss[1:4], lower = list(continuous = my_fn))
# Use wrap to add further arguments; change method to lm
ggpairs(swiss[1:4], lower = list(continuous = wrap(my_fn, method="lm")))


#OPtion 2
my_fn <- function(data, mapping, pts=list(), smt=list(), ...){
        ggplot(data = data, mapping = mapping, ...) + 
                do.call(geom_point, pts) +
                do.call(geom_smooth, smt) 
}

# Plot 
ggpairs(swiss[1:4], 
        lower = list(continuous = 
                             wrap(my_fn,
                                  pts=list(size=2, colour="red"), 
                                  smt=list(method="lm", se=F, size=5, colour="blue"))))

#Option 3
g = ggpairs(swiss, lower = list(continuous = wrap("smooth", method = "lm")))
g


#Calling 'lm'
summary(lm(Fertility~.,data=swiss))$coefficients
#This shows that our model estimates an expected 0.17 decrease in standardized
#fertility for every 1% increase in percentage of males involved in agriculture
#in holding the remaining variables constant (column 1, row 2 shows -0.17211)


#How can adjustment reverse the sign of an effect? Let's try a simulation
n<-100
x2<-1:n
x1<-0.01*x2+runif(n,-.1,.1)
y<- -x1+x2+rnorm(n,sd=0.01)
summary(lm(y~x1))$coef
summary(lm(y~x1+x2))$coef
#Note our coefficent is 95, and not close to -1 with the first approximation, 
#but works well in the second option

#Plot it
dat=data.frame(y=y,x1=x1,x2=x2,ey=resid(lm(y~x2)),ex1=resid(lm(x1~x2)))
g=ggplot(dat,aes(y=y,x=x1,colour=x2))
g=g+geom_point(colour="grey50",size=5)+
        geom_smooth(method=lm,se=FALSE,colour="black")+
        geom_point(size=4)
g
#There is some confounding

g2=ggplot(dat,aes(y=ey,x=ex1,colour=x2))
g2=g2+geom_point(colour="grey50",size=5)+
        geom_smooth(method=lm,se=FALSE,colour="black")+
        geom_point(size=4)
g2
#the x2 variable is clearly not related to the x1 variable


#Try an example where adding in a variable is unnessecary
z<-swiss$Agriculture+swiss$Education
lm(Fertility~.+z,data=swiss)
#Agriculture coeff didn't change, and the z variable got an NA coeff
#This means that it is redundant (a linear combination of the other variables)



#Example
require(datasets)
data("InsectSprays")
require(stats)
g=ggplot(data=InsectSprays,aes(y=count,x=spray,fill=spray))
g=g+geom_violin(colour="black",size=2)
g=g+xlab("Type of Spray")+ylab("Insect Count")
g

summary(lm(count~spray,data=InsectSprays))$coef
#Note that Spray A is missing from the table because it is what we are comparing
#the other sprays to. (The intercept will be Spray A's mean). 
#-12.41666 is the difference between spray A and spray C

#Hard coding the dummy variables
summary(lm(data=InsectSprays,
           count~I(1*(spray == 'B'))+I(1*(spray=='C'))+
                I(1*(spray == 'D'))+I(1*(spray=='E'))+
        I(1*(spray == 'F'))))$coef



#Example
library(datasets)
data(swiss)
library(dplyr)
#Create a binary Catholic column
swiss=mutate(swiss,CatholicBin=1*(Catholic>50))
g=ggplot(swiss, aes(x=Agriculture, y=Fertility, color=factor(CatholicBin)))
g=g+geom_point(size=2, colour="black")+geom_point()
g=g+xlab("% in Agriculture")+ylab("Fertility")
g
#Add model lines
fit=lm(Fertility~Agriculture, data=swiss)
g1=g
g1=g1+geom_abline(intercept=coef(fit)[1],slope=coef(fit)[2],size=2)
g1
#Different slopes based on religion
fit=lm(Fertility~Agriculture*factor(CatholicBin),data=swiss)
g1=g
g1=g1+geom_abline(intercept=coef(fit)[1],slope=coef(fit)[2],size=2)
g1=g1+geom_abline(intercept=coef(fit)[1]+coef(fit)[3],
                  slope=coef(fit)[2]+coef(fit)[4],size=2)
g1



#Adjustment Simulation 1
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


#Simulation 2
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

#Note the difference between the two simulations. Simulation 2 makes it looks
#you didn't randomize your control and experimental groups (because the blue and
#red dots don't have overlapping x values)

#Simulation 4
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(.5 + runif(n/2), runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

#Simulation 5
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2, -1, 1), runif(n/2, -1, 1)); 
beta0 <- 0; beta1 <- 2; tau <- 0; tau1 <- -4; sigma <- .2
y <- beta0 + x * beta1 + t * tau + t * x * tau1 + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t + I(x * t))
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

#Simulation 5 has no treatment effect

#Simulation 6
p <- 1
n <- 100; x2 <- runif(n); x1 <- p * runif(n) - (1 - p) * x2 
beta0 <- 0; beta1 <- 1; tau <- 4 ; sigma <- .01
y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)
plot(x1, y, type = "n", frame = FALSE)
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)
#There isn'y a relationship between y and x1
#recreate the above plot in 3d, but it fails
library(rgl)
plot3d(x1, x2, y)
#Residual relationship
plot(resid(lm(x1 ~ x2)), resid(lm(y ~ x2)), frame = FALSE, col = "black", bg = "lightblue", pch = 21, cex = 2)
abline(lm(I(resid(lm(x1 ~ x2))) ~ I(resid(lm(y ~ x2)))), lwd = 2)
#The perfect straightline says there is a strong relationship between y and x1
#after removing x2


#Residuals Diagnostics and Variation
data(swiss); 
par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)

#Outliers
n <- 100; x <- rnorm(n); y <- x + rnorm(n, sd = .3)
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE, xlab = "X", ylab = "Y")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)

#Tool to give you a list of potential influence measures (to see if it is
#an outlier)
?influence.measures

#List:
#Do ?influence.measures to see the full suite of influence measures in stats. The measures include

#rstandard - standardized residuals, residuals divided by their standard deviations)
#rstudent - standardized residuals, residuals divided by their standard deviations, where the ith data point was deleted in the calculation of the standard deviation for the residual to follow a t distribution
#hatvalues - measures of leverage
#dffits - change in the predicted response when the $i^{th}$ point is deleted in fitting the model.
#dfbetas - change in individual coefficients when the $i^{th}$ point is deleted in fitting the model.
#cooks.distance - overall change in the coefficients when the $i^{th}$ point is deleted.
#resid - returns the ordinary residuals
#resid(fit) / (1 - hatvalues(fit)) where fit is the linear model fit returns the PRESS residuals, i.e. the leave one out cross validation residuals - the difference in the response and the predicted response at data point $i$, where it was not included in the model fitting.

#Go through simulation experiments to understand how the diagnostic instruments
#work

#Case 1
x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))   
#There is a clear outlier point that creates a correlation to data that isn't
#correlated
#Diagnostics:
fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3)
round(hatvalues(fit)[1 : 10], 3)
#Notice that the first point is orders of magnitude higher than the rest

#Case 2
x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)
#There is an outlier that does fit in with the correlation
round(dfbetas(fit2)[1 : 10, 2], 3)
round(hatvalues(fit2)[1 : 10], 3)
#Diagnostics look much better for the dbetas(), but not so much for the 
#hatvalues()

#Great Example described by Stefanski TAS 2007 Vol 61.
## Don't everyone hit this server at once.  Read the paper first.
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)
#p-values
summary(lm(V1 ~ . -1, data = dat))$coef
#Should we Bother looking at the residuals?
fit <- lm(V1 ~ . - 1, data = dat); plot(predict(fit), resid(fit), pch = '.')
#Yup! It shows a pattern here that we wouldn't see otherwise to show poor model
#fit

#Back to swiss data
data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)
#Look for poor model fit
#Q-Q plot looks for normality

#Multivariable Regression: Choosing a Model
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 
betas <- sapply(1 : nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        #note y does not depend on x2 or x3
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)
#Note that including the extra variables doesn't change anything

#Now do an example where x2 and x3 are heavily dependent upon x1
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2); 
betas <- sapply(1 : nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)
#Now, including x2 and x3 and things change a lot (variance inflation)
#Variance becomes more inflated the more related the variables are


data(swiss); 
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- update(fit, Fertility ~ Agriculture + Examination)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
c(summary(fit2)$cov.unscaled[2,2],
  summary(fit3)$cov.unscaled[2,2]) / a 


library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)
sqrt(vif(fit)) #I prefer sd 


#Nested mdoel testing (test to see if the additional terms improve the model)
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)



#Quote: George Box: all models are wrong, but some models are useful


#Plots residuals vs. their fitted values to check for model fit (can change
#which to 2, 3, 4, or 5 for the other types of diagnostic plots)
plot(fit, which=1)


