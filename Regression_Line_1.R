library(readr)
library(stargazer)
library(jtools)


rm(list = ls())

#to upload file
mydal1=read.csv("C:/Users/bridg/OneDrive/Desktop/Econometric R Data/Alabama FRED HW.csv",header = TRUE)

head(mydal1)

mydal= subset(mydal1,YEAR>=1985)
View(mydal)

mydal$ALINC = mydal$ALINCOME/1000

# to plot linear regression
plot(x=mydal$ALINC, y=mydal$PCRIMER,xlab = "real household income",ylab = "AL property crime rate",main = "Scatter plot of Real Household Income and propert crime  rate for Alabama", pch=21, bg="blue")


reg1 = lm(PCRIMER ~ ALUR, data=mydal)
reg2 = lm(PCRIMER ~ ALUR + ALINC, data=mydal)
reg3 = lm(PCRIMER ~ ALUR + ALINC + ALHOWN, data=mydal)
summ(reg1, digits = getOption("jtools-digits",4))
summ(reg2, digits = getOption("jtools-digits",4))
summ(reg3, digits = getOption("jtools-digits",4))

# statistical summary
stargazer(mydal, type = 'text')
