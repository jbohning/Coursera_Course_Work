library(UsingR)
library(ggplot2)

data(diamond)
plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(lm(price ~ carat, data = diamond), lwd = 2)

#Fitting the linear regression model
fit <- lm(price ~ carat, data = diamond)
coef(fit)

#Same thing, using ggplot2
g=ggplot(diamond,aes(x=carat,y=price))
g=g+xlab("Mass (carats)")
g=g+ylab("Price (SIN  $)")
g=g+geom_point(size=5,colour="blue",alpha=0.2)
g=g+geom_smooth(method="lm",colour="black")
g

#Getting a more interpretable intercept (mean center)
fit2<-lm(price~I(carat-mean(carat)),data=diamond)
coef(fit2)

#Third intercept option:
fit3<-lm(price~I(carat*10),data=diamond)
coef(fit3)

#Plugging data into regression to find y values (predicting price based on a 
#given value of carat)
predict(fit,newdata=data.frame(carat=c(0.16,0.27,0.34)))

#Calculating Residuals
y<-diamond$price
x<-diamond$carat
n<-length(y)
fit<-lm(y~x)
e<-resid(fit)
#Second way to get resids:
yhat<-predict(fit)
max(abs(e-(y-yhat)))
max(abs(e-(y-coef(fit)[1]-coef(fit)[2]*x)))

#plot it with residuals (this is a cool graph)
plot(diamond$carat,diamond$price,xlab="Mass (carats)",ylab="Price (SIN $)",
     col="black", cex=1.1,pch=21,frame=FALSE)
abline(fit,lwd=2)
for (i in 1:n)
        lines(c(x[i],x[i]),c(y[i],yhat[i]),col="red",lwd=2)

#Plot residuals on the y axis and the mass on the x (want to see random lines)
plot(x,e,xlab="Mass (carats)",ylab="Residuals (SIN$)",bg="lightblue",
    col="black",cex=2,pch=21,frame=FALSE)
abline(h=0,lwd=2)
for (i in 1:n)
        lines(c(x[i],x[i]),c(e[i],0),col="red",lwd=2)


#Non-linear data example (using a linear model)
x=runif(100,-3,3);y=x+sin(x)+rnorm(100,sd=.2)
g=ggplot(data.frame(x=x,y=y),aes(x=x,y=y))
g=g+geom_smooth(method="lm",colour="black")
g=g+geom_point(size=5,colour="red",alpha=0.4)
g
#now plot the residual
g=ggplot(data.frame(x=x,y=resid(lm(y~x))),aes(x=x,y=y))
g=g+geom_hline(yintercept=0,size=2)
g=g+geom_point(size=5,colour="red",alpha=0.4)
g=g+xlab("X")+ylab("Residual")
g
#It becomes apparent that there is a slight-non-linear component to this data


#Heteroskedasticity example (megaphone)
x<-runif(100,0,6);y<-x+rnorm(100,mean=0,sd=.001*x)
g=ggplot(data.frame(x=x,y=y),aes(x=x,y=y))
g=g+geom_smooth(method="lm",colour="black")
g=g+geom_point(size=5,colour="red",alpha=0.4)
g
#Can't see anything wrong with using a linear model until you plot the residuals:
g=ggplot(data.frame(x=x,y=resid(lm(y~x))),aes(x=x,y=y))
g=g+geom_hline(yintercept=0,size=2)
g=g+geom_point(size=5,colour="red",alpha=0.4)
g=g+xlab("X")+ylab("Residual")
g
#Note that the residuals grow as x increases


#Diamond example again using a dotplot
e=c(resid(lm(price~1,data=diamond)),resid(lm(price~carat,data=diamond)))
fit=factor(c(rep("itc",nrow(diamond)),rep("Itc, slope",nrow(diamond))))
g=ggplot(data.frame(e=e,fit=fit),aes(y=e,x=fit,fill=fit))
g=g+geom_dotplot(binaxis="y",stackdir="center",binwidth=30)
g=g+xlab("Fitting approach")+ylab("Residual price")
g


#Residual Variation
y<-diamond$price
x<-diamond$carat
n<-length(y)
fit<-lm(y~x)
summary(fit)$sigma
#Option 2
sqrt(sum(resid(fit)^2)/(n-2))


#Coding Exmaple on Inference
y<-diamond$price
x<-diamond$carat
n<-length(y)
beta1<-cor(y,x)*sd(y)/sd(x)
beta0<-mean(y)-beta1*mean(x)
e<-y-beta0-beta1*x
sigma<-sqrt(sum(e^2)/(n-2))
ssx<-sum((x-mean(x))^2)
seBeta0<-(1/n+mean(x)^2/ssx)^.5*sigma
seBeta1<-sigma/sqrt(ssx)
tBeta0<-beta0/seBeta0
tBeta1<-beta1/seBeta1
pBeta0<-2*pt(abs(tBeta0),df=n-2,lower.tail=FALSE)
pBeta1<-2*pt(abs(tBeta1),df=n-2,lower.tail=FALSE)
coefTable<-rbind(c(beta0,seBeta0,tBeta0,pBeta0),c(beta1,seBeta1,tBeta1,pBeta1))
colnames(coefTable)<-c("Estimate","Std.Error","t value","P(>|t|)")
rownames(coefTable)<-c("(Intercept)","x")
coefTable
#easy way to do it
fit<-lm(y~x)
summary(fit)$coefficients

#Get a confindence intevcal
summary(fit) #Gives you p-value in this chunk
sumCoef<-summary(fit)$coefficients
sumCoef[1,1]+c(-1,1)*qt(.975,df=fit$df)*sumCoef[1,2] #CI on intercept
sumCoef[2,1]+c(-1,1)*qt(.975,df=fit$df)*sumCoef[2,2] #CI on slope

#What this is saying is: with 95% confidence, we estimate that a 1 carat
#increase in diamond size is going to result in a $3556.398 to $3885.651
#increase in cost


#Prediction Intervals
newx<-data.frame(x=seq(min(x),max(x),length=100))
p1<-data.frame(predict(fit,newdata=newx,interval=("confidence")))
p2<-data.frame(predict(fit,newdata=newx,interval=("prediction")))
p1$interval="confidence"
p2$interval="prediction"
p1$x<-newx$x
p2$x<-newx$x
dat<-rbind(p1,p2)
names(dat)[1]="y"

g=ggplot(dat,aes(x=x,y=y))
g=g+geom_ribbon(aes(ymin=lwr,ymax=upr,fill=interval),alpha=0.2)
g=g+geom_line()
g=g+geom_point(data=data.frame(x=x,y=y),aes(x=x,y=y),size=4)
g









