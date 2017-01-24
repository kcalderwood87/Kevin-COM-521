#Find how many rows and columns there are in the data set

dim(week3_dataset_kevin)

#Identify any missing values

is.na(week3_dataset_kevin)

#Create a data frame for week 3 data set 
data.frame(week3_dataset_kevin)

#Identify the names of the columns for week 3 data set
names(week3_dataset_kevin)

#Find the appropriate summary statistics and a histogram for variable x

summary(week3_dataset_kevin$x)
sd(week3_dataset_kevin$x)
IQR(week3_dataset_kevin$x)
hist(week3_dataset_kevin$x)

#Find the appropriate summary statistics for variable j

summary(week3_dataset_kevin$j)
sort(table(week3_dataset_kevin$j))
hist(week3_dataset_kevin$j)

#Find the appropriate summary statistics for variable i

summary(week3_dataset_kevin$i)
sort(table(week3_dataset_kevin$i))
hist(week3_dataset_kevin$i)

#Find the appropriate summary statistics for variable k

summary(week3_dataset_kevin$k)
sort(table(week3_dataset_kevin$k))
hist(week3_dataset_kevin$k)

#Find the appropriate summary statistics for variable y

summary(week3_dataset_kevin$y)
sd(week3_dataset_kevin$y)
IQR(week3_dataset_kevin$y)
hist(week3_dataset_kevin$y)

#Compare the week2.dataset with the first column of data frame

summary(week2.dataset)
summary(week3_dataset_kevin$x)
a <- data.frame(week2.dataset)
b <- data.frame(week3_dataset_kevin$x)
install.packages("compare")
library(compare)
compare(a, b, ignoreNames=T)
#The week2.dataset vector is not equivalent to the x column in week3 dataset

#Visualize the data using ggplot2

install.packages("ggplot2")
library(ggplot2)
ggplot(data=week3_dataset_kevin) + geom_point() + aes(x=x, y=y, color=i, shape=as.logical(j), size=k)

#Recode the data

tmp.week3_dataset <- week3_dataset_kevin

i <- as.logical(tmp.week3_dataset$i)
list(i)
class(i)

j <- as.logical(tmp.week3_dataset$j)
list(j)
class(j)

k <- factor(tmp.week3_dataset$k, labels = c("none", "some", "lots", "all"))
list(k)
class(k)

#Recode the data and then revert it back

i[i == FALSE] <- NA
list(i)
i[is.na(i)] <- 0
list(i)
i <- as.logical(tmp.week3_dataset$i)
list(i)

#Find the appropriate summary statistics for variable j

summary(j)
sort(table(j))
hist(j) #Error must be numeric

#Find the appropriate summary statistics for variable i

summary(i)
sort(table(i))
hist(i) #Error must be numeric

#Find the appropriate summary statistics for variable k

summary(k)
sort(table(k))
hist(k) #Error must be numeric
