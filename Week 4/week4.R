###PC2

#Check number of columns and rows
ncol(COS_Statistics_Mobile_Sessions)
nrow(COS_Statistics_Mobile_Sessions)

#Check number of columns and rows

ncol(COS_Statistics_Top5000_Pages)
nrow(COS_Statistics_Top5000_Pages)

#Check for missing values

is.na(COS_Statistics_Mobile_Sessions)
is.na(COS_Statistics_Top5000_Pages)

#Create a random subsample and then look at the data

MobileSessionsSample1 <- COS_Statistics_Mobile_Sessions[sample(1:nrow(COS_Statistics_Mobile_Sessions), 50, replace=FALSE),]
list(MobileSessionsSample1)
MobileSessionsSample2 <- COS_Statistics_Mobile_Sessions[sample(1:nrow(COS_Statistics_Mobile_Sessions), 50, replace=FALSE),]
list(MobileSessionsSample2)
Top5000sample1 <- COS_Statistics_Top5000_Pages[sample(1:nrow(COS_Statistics_Top5000_Pages), 50, replace=FALSE),]
list(Top5000sample1)
Top5000sample2 <- COS_Statistics_Top5000_Pages[sample(1:nrow(COS_Statistics_Top5000_Pages), 50, replace=FALSE),]
list(Top5000sample2)

###PC3

#Create a data frame for the number of page views per month

objects(COS_Statistics_Top5000_Pages)
tapply(COS_Statistics_Top5000_Pages$Pageviews, COS_Statistics_Top5000_Pages$Month, sum)
Monthlypageviews = aggregate(COS_Statistics_Top5000_Pages$Pageviews, list(COS_Statistics_Top5000_Pages$Month), sum)
class(Monthlypageviews)
list(Monthlypageviews)

###PC4

#Create a data frame with an estimate for the number of mobile page views per month

objects(COS_Statistics_Mobile_Sessions)
tapply(COS_Statistics_Mobile_Sessions$Sessions*COS_Statistics_Mobile_Sessions$PagesPerSession, COS_Statistics_Mobile_Sessions$Month, sum)
Mobilepageviews = aggregate(COS_Statistics_Mobile_Sessions$Sessions*COS_Statistics_Mobile_Sessions$PagesPerSession, list(COS_Statistics_Mobile_Sessions$Month), sum)
class(Mobilepageviews)
list(Mobilepageviews)

###PC5

#Merge the two datasets together

Mergedviews = merge(Monthlypageviews, Mobilepageviews, by = "Group.1")
list(Mergedviews)
is.na(Mobilepageviews)
is.na(Monthlypageviews)

###PC6

#Create a new column with the percentage of mobile page views

Mergedviews$Percentage <- (Mergedviews$x.y / Mergedviews$x.x)*100
list(Mergedviews)

#Make a datetime object in R and order the dataframe by date

Mergedviews$Group.1 <- gsub(x=Mergedviews$Group.1, pattern = "12:00:00 AM", replacement = "", fixed = T)
Mergedviews$Group.1 <- as.Date(Mergedviews$Group.1, "%m/%d/%Y")
list(Mergedviews$Group.1)
class(Mergedviews$Group.1)
Mergedviews <- Mergedviews[order(as.Date(Mergedviews$Group.1, format="%Y/%m/%d")),]
list(Mergedviews)

###PC6 Part 2

#Plot the change over time in mobile views

library(ggplot2)
ggplot(data=Mergedviews) + geom_point() + aes(x=Group.1, y=Percentage)