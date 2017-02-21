#PC0 Load the Week 3 Dataset

library(readr)
week3_dataset_kevin <- read_csv("~/Google Drive/Kevin-COM-521/Week 3/week3_dataset-kevin.csv")
View(week3_dataset_kevin)

#PC1

#Run a t-test on x and y. 

t.test(week3_dataset_kevin$x, week3_dataset_kevin$y)

###The results reveal that their relationship is statistically significant at the p<.000 level.

#PC2 I would estimate that x and y are highly correlated with one another.  

plot(week3_dataset_kevin$x, week3_dataset_kevin$y)
cor(week3_dataset_kevin$x, week3_dataset_kevin$y) #.872 correlation
cor(week3_dataset_kevin$x, week3_dataset_kevin$y, method="spearman") #.854 correlation

###The correlation function shows a correlation coeffecient of .872

#PC3 Recode the data (this is from Week 3)

tmp.week3_dataset <- week3_dataset_kevin
class(tmp.week3_dataset$x)
x <- tmp.week3_dataset$x
y <- tmp.week3_dataset$y
i <- as.logical(tmp.week3_dataset$i)
i
class(i)
j <- as.logical(tmp.week3_dataset$j)
j
class(j)
k <- factor(tmp.week3_dataset$k, labels = c("none", "some", "lots", "all"))
k
class(k)

#PC4 Create linear models

yxlm <- lm(y ~ x, data = tmp.week3_dataset)
yxlm.i <- lm(y ~ x + i, data = tmp.week3_dataset)
yxlm.ij <- lm(y ~ x + i + j, data = tmp.week3_dataset)
yxlm.full <- lm(y ~ x + i + j + k, data = tmp.week3_dataset) 

#Show a table of the results of the models

summary(yxlm)
summary(yxlm.i)
summary(yxlm.ij)
summary(yxlm.full)

#Find the r^2 for the models

summary(yxlm)$r.squared
summary(yxlm.i)$r.squared
summary(yxlm.ij)$r.squared
summary(yxlm.full)$r.squared

#PC5 Create histograms and plots for the residuals

#Histogram

hist(residuals(yxlm.full))

#Plots

plot(x, residuals(yxlm.full))
plot(i, residuals(yxlm.full))
plot(j, residuals(yxlm.full))
plot(k, residuals(yxlm.full))

#QQNorm

qqnorm(residuals(yxlm.full))

#PC6 Create a publication ready table

library(stargazer)
stargazer(yxlm.full, type = "text")

#PC7 Load the data set and write linear models

library(haven)
Halloween2012_2014_2015_PLOS <- read_dta("~/Google Drive/Kevin-COM-521/Week 7/Halloween2012-2014-2015_PLOS.dta")
View(Halloween2012_2014_2015_PLOS)

#PC7a

fruit <- Halloween2012_2014_2015_PLOS$fruit
obama <- Halloween2012_2014_2015_PLOS$obama
fruit.obama <- lm(fruit ~ obama, data = Halloween2012_2014_2015_PLOS)
fruit.obama
summary(fruit.obama)

###None of the variables are significant, and the model only accounts for less than 1% of the variance. 

#P7b. Create a linear model for fruit by obama with age and year as control variables. 

age <- Halloween2012_2014_2015_PLOS$age
year <- as.factor(Halloween2012_2014_2015_PLOS$year)
control.fruit.obama <- lm(fruit ~ obama + age + year, data = Halloween2012_2014_2015_PLOS)
control.fruit.obama
summary(control.fruit.obama)

###None of the variables are significant, and the model only accounts for less than 1% of the variance. 

#PC8

hist(residuals(fruit.obama))
plot(residuals(fruit.obama))
qqnorm(residuals(fruit.obama))

fruit.obama.graph <- data.frame(x = Halloween2012_2014_2015_PLOS$fruit, residuals = residuals(fruit.obama))
yxlm.graph
library(ggplot2)
ggplot(data=yxlm.graph) + aes(x=x, y=residuals) +geom_point()

#PC9 Create subsets of the data for 2012, 2014, and 2015 and run model

d.2012 <- subset(Halloween2012_2014_2015_PLOS, year == 2012)
d.2014 <- subset(Halloween2012_2014_2015_PLOS, year == 2014)
d.2015 <- subset(Halloween2012_2014_2015_PLOS, year == 2015)

fruit.2012 <- d.2012$fruit
obama.2012 <- d.2012$obama
fruit.2014 <- d.2014$fruit
obama.2014 <- d.2014$obama
fruit.2015 <- d.2015$fruit
obama.2015 <- d.2015$obama

###2012 model

fruit.obama.2012 <- lm(fruit.2012 ~ obama.2012, data = d.2012)
fruit.obama.2012
summary(fruit.obama.2012)

###2014 model

fruit.obama.2014 <- lm(fruit.2014 ~ obama.2014, data = d.2014)
fruit.obama.2014
summary(fruit.obama.2014)

#2015 model

fruit.obama.2015 <- lm(fruit.2015 ~ obama.2015, data = d.2015)
fruit.obama.2015
summary(fruit.obama.2015)