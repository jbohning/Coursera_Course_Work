# The U.S. Environmental Protection Agency (EPA) sets national ambient air
# quality standards for outdoor air pollution

# U.S. National Ambient Air Quality Standards
# For fine particle pollution (PM2.5), the "annual mean, averaged over 3 years"
# cannot exceed $12~\mu g/m^3$.

# Data on daily PM2.5 are available from the U.S. EPA web site

# Question: Are there any counties in the U.S. that exceed that national standard
# for fine particle pollution?

pollution<- read.csv("/Users/JessicaBohning/Documents/Data Science/Course 4- Exploratory Data Analysis/Week 1/avgpm25.csv",
                     colClasses=c("numeric","character","factor","numeric","numeric"))

#Make a boxplot of particle pollutions
boxplot(pollution$pm25,col="blue")

#Make a histogram
hist(pollution$pm25,col="green")
rug(pollution$pm25)
#Note: the rug adds a little set of lines at the bottom to show where all of the
#data is

#Can change the bar widths on histogram:
hist(pollution$pm25,col="green", breaks=100)
rug(pollution$pm25)

# Can add a line to the boxplot to show where the national average is
boxplot(pollution$pm25,col="blue")
abline(h=12)
# This makes it clear that over 75% of the data is below the 12 max

#Can add lines on histograms too (added a line for the 12 standard and a line
#for the median of the data)
hist(pollution$pm25,col="green")
abline(v=12,lwd=2)
abline(v=median(pollution$pm25),col="magenta",lwd=4)

#Barplots: plotting the number of counties in each region
barplot(table(pollution$region),col="wheat",
        main="Number of Countries in Each Region")

#Multiple Boxplots
boxplot(pm25~region,data=pollution,col="red")

#Multiple Historgrams
par(mfrow=c(2,1),mar=c(4,4,2,1))
hist(subset(pollution,region=="east")$pm25,col="green")
hist(subset(pollution,region=="west")$pm25,col="green")

#Scatterplot
with(pollution,plot(latitude,pm25))
abline(h=12,lwd=2,lty=2)

#Add color
with(pollution,plot(latitude,pm25,col=region))
abline(h=12,lwd=2,lty=2)

#Multiple Scatterplots
par(mfrow=c(1,2),mar=c(5,4,2,1))
with(subset(pollution,region=="west"),plot(latitude,pm25,main="West"))
with(subset(pollution,region=="east"),plot(latitude,pm25,main="East"))

#Lattice Plot
library(lattice)
state<-data.frame(state.x77,region=state.region)
xyplot(Life.Exp~Income | region, data=state,layout=c(4,1))

# Using GGPlot2
library(ggplot2)
data(mpg)
qplot(displ,hwy,data=mpg)

# Second boxplot example
library(datasets)
airquality<- transform(airquality, Month=factor(Month))
boxplot(Ozone~Month, airquality,xlab="month",ylab="Ozone (ppb)")

#Base Plot with Annotation
library(datasets)
with(airquality, plot(Wind, Ozone, main="Ozone and Wind in New York City"))
with(subset(airquality, Month==5), points(Wind, Ozone, col="blue"))

#Base Plot with Annotation
library(datasets)
with(airquality, plot(Wind, Ozone, main="Ozone and Wind in New York City", 
                      type="n"))
with(subset(airquality, Month==5), points(Wind, Ozone, col="blue"))
with(subset(airquality, Month!=5), points(Wind, Ozone, col="red"))
legend("topright",pch=1, col=c("blue","red"),legend=c("May","Other Months"))

#Base Plot with Regression Line
with(airquality, plot(Wind, Ozone, main="Ozone and Wind in New York City", 
                      pch=20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd=2)

# Multiple Base Plots
par(mfrow=c(1,2))
with (airquality, {
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R,Ozone, main = "Ozone and Solar Radiation")
})

#Multiple Base Plots with overarching title
par(mfrow=c(1,3), mar=c(4,4,2,1), oma=c(0, 0, 2, 0))
with(airquality,{
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R,Ozone, main = "Ozone and Solar Radiation")
        plot(Temp,Ozone, main = "Ozone and Temperature")
        mtext("Ozone and Weather in New York City", outer=TRUE)
})


#To pull up the plot symbol code list, or see plotting demos
example(points)


 

#Simple Lattice ScatterPlots
library(lattice)
library(datasets)
xyplot(Ozone~Wind,data=airquality)

#Simple Lattice Plot
airquality<-transform(airquality,Month=factor(Month))
xyplot(Ozone~Wind|Month,data = airquality,layout=c(5,1))

#Prevent printing of graph
p<-xyplot(Ozone~Wind,data=airquality)
print(p)

#Lattice Panel Functions
set.seed(10)
x<-rnorm(100)
f<-rep(0:1,each=50)
y<- x+f-f*x+rnorm(100,sd=0.5)
f<-factor(f,labels = c("Group 1","Group 2"))
xyplot(y~x|f,layout=c(2,1)) #Plot with 2 panels

#Lattice Panel Functions
#Custom panel function
xyplot(y~x|f,panel=function(x,y,...){
        panel.xyplot(x,y,...) #First call the default panel funtion for 'xplot'
        panel.abline(h=median(y),lty=2) #Add a horizontal line at the median
})

#Lattice Panel Functions: Regression Lines
#Custom panel function
xyplot(y~x|f,panel=function(x,y,...){
        panel.xyplot(x,y,...) #First call the default panel funtion for 'xplot'
        panel.lmline(x,y,col=2) #Overlay a simple linear regression line
})

#To use ggplot2 functions like qplot (think quick plot), your data needs to be in
#a data frame. Note: qplot() works a lot like plot(). 
#Factors are important for indicating subsets of the data

library(ggplot2)

#Basic ggplot2
qplot(displ,hwy,data=mpg)

#Modifying aesthetics
qplot(displ, hwy, data = mpg, color=drv)
#note: color=drv sets the 3 types of drv variables to different colors

#Adding a geom (a smoother for 95% confidence interval)
qplot(displ,hwy,data=mpg,geom=c("point","smooth"))

# Histogram
qplot(hwy, data=mpg,fill=drv)
#fill=drv sets the fill colors to the three variables instead drv

#Facets (are like lattice plots). Scatterplot ex
qplot(displ,hwy,data=mpg,facets=.~drv)

#Facets: histogram example
qplot(hwy, data=mpg,facets=drv~.,binwidth=2)

#Density smooth
qplot(hwy,data=mpg,geom="density")

#Density smooth by variable group
qplot(hwy,data=mpg,geom="density",color=drv)


#Scatterplots: modifying shapes by variable
qplot(displ, hwy, data=mpg,shape=drv)

#Smooth the relationship for all three variables independently
qplot(displ, hwy, data=mpg,color=drv)+geom_smooth(method="lm")

#Smooth and split based on variable
qplot(displ, hwy, data=mpg,facets=.~drv)+geom_smooth(method="lm")

#Building plots in ggplot using layers
g<-ggplot(mpg, aes(displ,hwy))
g+geom_point()+geom_smooth(method="lm")
#Option 2
g+geom_point()+facet_grid(.~drv)+geom_smooth(method="lm")

#Modifying Aesthetics
g+geom_point(color="steelblue",size=4,alpha=1/2)

#Modifying Aesthetics #2
g+geom_point(aes(color=drv),size=4,alpha=1/2)
#Note: aes() stands for aesthetics

#Modifying labels
g+geom_point(aes(color=drv))+labs(title="Driving Data")+labs(x=expression("Displ "*displ[2.5]),y="Highway Gas Mileage")

#Cusomizing the Smooth
g+geom_point(aes(color=drv),size=2,alpha=1/2)+
        geom_smooth(size=4,linetype=3,method="lm",se=FALSE)

#Changing the Theme
g+geom_point(aes(color=drv))+theme_bw(base_family = "Times")



#Notes about Axis limits:
testdat<-data.frame(x=1:100,y=rnorm(100))
testdat[50,2]<-100 #OUTLIER
plot(testdat$x, testdat$y, type="l",ylim=c(-3,3))
#Note: ylim prevents graph from showing all values
g<-ggplot(testdat, aes(x=x,y=y))
g+geom_line()+ylim(-3,3)
#The above doesn't do the same thinig- it subsets the data from -3 to 3
g+geom_line()+coord_cartesian(ylim=c(-3,3))
#The above changes the y-axis properly

#Boxplot in ggplot2
qplot(drv,hwy,data=mpg,geom="boxplot")

#Boxplot #2
qplot(drv,hwy,data=mpg,geom="boxplot",color=manufacturer)

#Cool grid plot
g<-ggplot(data=mpg,aes(x=displ,y=hwy,color=factor(year)))
g+geom_point()+facet_grid(drv~cyl,margins=TRUE)+
        geom_smooth(method="lm",se=FALSE,size=2,color="black")+
        labs(x="Displacement",y="Highway Mileage",title="Swirl Rules!")

#Split up data
cutpoints<-quantile(diamonds$carat,seq(0,1,length=4),na.rm=TRUE)
diamonds$car2<-cut(diamonds$carat,cutpoints)
g<-ggplot(data=diamonds,aes(depth,price))
g+geom_point(alpha=1/3)+facet_grid(cut~car2)

#Another boxplot
ggplot(diamonds,aes(carat,price))+geom_boxplot()+facet_grid(.~cut)


#How to spread out points that lie on top of each other
library(UsingR)
plot(jitter(child,4)~parent,galton)
