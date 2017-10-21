#Regression Modeling using Galton's parent/child height data

#install.packages("UsingR")
library(UsingR)
library(reshape)
library(ggplot2)
data("galton")

#Data exploration on the two population's data
long<-melt(galton)
g<-ggplot(long,aes(x=value,fill=variable))
g<-g+geom_histogram(colour="black",binwidth = 1)
g<-g+facet_grid(.~variable)
g

#Finding the Center of Mass for the Data
library(manipulate)
myHist<-function(mu){
        mse<-mean((galton$child-mu)^2)
        g<-ggplot(galton,aes(x=child))+
                geom_histogram(fill="salmon",color="black",binwidth=1)+
                geom_vline(xintercept = mu,size=3)+
                ggtitle(paste("mu = ",mu, ", MSE=", round(mse,2), sep=""))
        g
}
manipulate(myHist(mu),mu=slider(62,74,step=0.5))

#Notice that at mu=8, the MSE (or mean square error) is the smallest


#Another exploratory graph (a very flawed graph)
ggplot(galton, aes(x=parent, y=child))+geom_point()

#A better example of the same plot (which changes the size of points depending
#on how many data points fall on that spot)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .15 * freqData$freq, 
     xlab = "Parent Height (inches)", ylab = "Child Height (inches)")


#Finding a line of best fit (forces the line of best fit to go through the origin)
myPlot <- function(beta){
        y <- galton$child - mean(galton$child)
        x <- galton$parent - mean(galton$parent)
        freqData <- as.data.frame(table(x, y))
        names(freqData) <- c("child", "parent", "freq")
        plot(
                as.numeric(as.vector(freqData$parent)), 
                as.numeric(as.vector(freqData$child)),
                pch = 21, col = "black", bg = "lightblue",
                cex = .15 * freqData$freq, 
                xlab = "parent", 
                ylab = "child"
        )
        abline(0, beta, lwd = 3)
        points(0, 0, cex = 2, pch = 19)
        mse <- mean( (y - beta * x)^2 )
        title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

#How to find the actual slope line:
lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)
#output: slope is 0.6463


#Linear Least Squares Coding Example (gives x-intercept and slope)
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))

#Also, a quick way to do a linear regression line of best fit
coef(lm(y~x))


#Adding the line of best fit to the graph
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .05 * freqData$freq, 
     xlab = "parent", ylab = "child", xlim = c(62, 74), ylim = c(62, 74))
abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), sd(y) / sd(x) * cor(y, x), lwd = 3, col = "red")
abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), sd(y) / sd(x) / cor(y, x), lwd = 3, col = "blue")
abline(mean(y) - mean(x) * sd(y) / sd(x), sd(y) / sd(x), lwd = 2)
points(mean(x), mean(y), cex = 2, pch = 19)




#Regression to the mean examples

#Normalizing the dta and setting plotting parameters
library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
myPlot <- function(x, y) {
        plot(x, y, 
             xlab = "Father's height, normalized",
             ylab = "Son's height, normalized",
             xlim = c(-3, 3), ylim = c(-3, 3),
             bg = "lightblue", col = "black", cex = 1.1, pch = 21, 
             frame = FALSE)
}
#Plot the data, code
myPlot(x, y)
abline(0, 1) # if there were perfect correlation
abline(0, rho, lwd = 2) # father predicts son
abline(0, 1 / rho, lwd = 2) # son predicts father, son on vertical axis
abline(h = 0); abline(v = 0) # reference lines for no relathionship

#Plot option2:
ggplot(data.frame(x=x,y=y),aes(x=x,y=y))+
        geom_point(size=6,colour="black",alpha=0.2)+
        geom_point(size=4,colour="salmon",alpha=0.2)+
        xlim(-4,4)+ylim(-4,4)+
        geom_abline(intercept=0,slope=1)+
        geom_vline(xintercept=0)+
        geom_hline(yintercept=0)+
        geom_abline(intercept=0,slope=rho,size=2)+
        geom_abline(intercept=0,slope=1/rho,size=2)














