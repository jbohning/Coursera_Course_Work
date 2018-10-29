#instrumental variables example


#install package
install.packages("ivpack")
#load package
library(ivpack)

#read dataset
data(card.data)

#IV is nearc4 (near 4 year college)
#outcome is lwage (log of wage)
#'treatment' is educ (number of years of education)

#summary stats
mean(card.data$nearc4)
par(mfrow=c(1,2))
hist(card.data$lwage)
hist(card.data$educ)

#is the IV associated with the treatment? strenght of IV
mean(card.data$educ[card.data$nearc4==1])
mean(card.data$educ[card.data$nearc4==0]) 


#make education binary
educ12<-card.data$educ>12   #binary variable - more than 12 years of education
#estimate proportion of 'compliers'
propcomp<-mean(educ12[card.data$nearc4==1])-
  mean(educ12[card.data$nearc4==0])
propcomp

#intention to treat effect
itt<-mean(card.data$lwage[card.data$nearc4==1])-
  mean(card.data$lwage[card.data$nearc4==0])
itt

#complier average causal effect (if this is larger than intent to treat effec then 
#there aren't defiers ie people who do the oppotive of what treatment they are assigned, 
#or in this case people who don't live near a 4 year college that do end up going to
#college)
itt/propcomp


#two stage least squares
#(you can do the same analysis using two stage least squares)
#stage 1: we regress treatment on the instrument ie regress education on living near
# a four year college


#stage 1: regress A on Z
s1<-lm(educ12~card.data$nearc4)
## get predicted value of A given Z for each subject
predtx <-predict(s1, type = "response")
table(predtx) #output: people who don't live near the college had a predicted probability
#of going to college of 0.422 while people who do live near a college had a predicted 
#probability of going to college of 0.544

#stage 2: regress Y on predicted value of A
lm(card.data$lwage~predtx)


#You can also do two-stage lease squares with the ivpack package
#2SLS using ivpack
ivmodel=ivreg(lwage ~ educ12, ~ nearc4, x=TRUE, data=card.data)
robust.se(ivmodel)


#Now control for other variables (like how living near a college might mean higher
#rents and therefore higher incomes)
ivmodel=ivreg(lwage ~ educ12 + exper + reg661 + reg662 +
                reg663 + reg664 + reg665+ reg666 + reg667 + reg668, 
              ~ nearc4 + exper +
                reg661+ reg662 + reg663 + reg664 + reg665 + reg666 +
                reg667 + reg668, x=TRUE, data=card.data)



