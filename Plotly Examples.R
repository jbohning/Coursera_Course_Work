#Example 1
data(mtcars)
library(plotly)
plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter")

#Scatterplot with color
plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", color = ~factor(cyl))

#Scatterplot with continuous color
plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", color = ~disp)

#Scatterplot sizing
plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", 
        color = ~factor(cyl), size = ~hp)

#3d Plot
set.seed(2016-07-21)
temp <- rnorm(100, mean = 30, sd = 5)
pressue <- rnorm(100)
dtime <- 1:100
plot_ly(x = ~temp, y = ~pressue, z = ~dtime,
        type = "scatter3d", color = ~temp)


#Line Graph
data("airmiles")
plot_ly(x = ~time(airmiles), y = ~airmiles, type = "scatter", mode = "lines")


#Multiline Graph
library(plotly)
library(tidyr)
library(dplyr)
data("EuStockMarkets")

stocks <- as.data.frame(EuStockMarkets) %>%
        gather(index, price) %>%
        mutate(time = rep(time(EuStockMarkets), 4))

plot_ly(stocks, x = ~time, y = ~price, color = ~index, type = "scatter", mode = "lines")


#Histogram
plot_ly(x = ~precip, type = "histogram")

#Boxplot
plot_ly(iris, y = ~Petal.Length, color = ~Species, type = "box")


#Heatmap
terrain1 <- matrix(rnorm(100*100), nrow = 100, ncol = 100)
plot_ly(z = ~terrain1, type = "heatmap")

#3D Surface
terrain2 <- matrix(sort(rnorm(100*100)), nrow = 100, ncol = 100)
plot_ly(z = ~terrain2, type = "surface")


#Choropeth Map Setup
# Create data frame
state_pop <- data.frame(State = state.abb, Pop = as.vector(state.x77[,1]))
# Create hover text
state_pop$hover <- with(state_pop, paste(State, '<br>', "Population:", Pop))
# Make state borders white
borders <- list(color = toRGB("red"))
# Set up some mapping options
map_options <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
)
plot_ly(z = ~state_pop$Pop, text = ~state_pop$hover, locations = ~state_pop$State, 
        type = 'choropleth', locationmode = 'USA-states', 
        color = state_pop$Pop, colors = 'Blues', marker = list(line = borders)) %>%
        layout(title = 'US Population in 1975', geo = map_options)


#ggplot example
library(ggplot2)
set.seed(100)
d<-diamonds[sample(nrow(diamonds),1000),]
p<-ggplot(data=d,aes(x=carat,y=price))+
        geom_point(aes(text=paste("Clarity:",clarity)),size=4)+
        geom_smooth(aes(colour=cut,fill=cut))+facet_wrap(~cut)
(gg<-ggplotly(p))











