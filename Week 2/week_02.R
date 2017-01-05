#Locate the variable in the dataset

objects()

#Check for missing values

is.na(week2.dataset)

#Calculate the mean for the dataset

mean(week2.dataset) #Mean = 34.99619

#Calculate the median for the dataset

median(week2.dataset) #Median = 39.13345

#Calculate the variance for the dataset

var(week2.dataset) #Var = 1687.363

#Calculate the standard deviation for the dataset

sd(week2.dataset) #SD = 41.07753 

#Calculate the interquartile range for the dataset

IQR(week2.dataset) #IQR = 47.60001

#Calculate the summary statistics for the dataset

summary(week2.dataset) # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-69.49   11.46   39.13   35.00   59.06  138.90 

#Calculate the mean without the mean function

length(week2.dataset) #Length = 100 cases
sum(week2.dataset) #Sum of all values in dataset = 3499.619
3499.619/100 #Mean = 34.99619

#Calculate the median without the median function

sort(week2.dataset) #Sort the dataset from smallest to largest. The two middlemost numbers are 38.7266822 and 39.5402196.
38.7266822+39.5402196 #Add the numbers = 78.269 and then divide by 2.
78.2669/2 #Median = 39.13345.

#Calculate the mode.

sort(table(week2.dataset)) #Create a table, and then sort the dataset from smallest number of occurances to largest number of occurances in the dataset. The 100 cases all appear exactly once. 

#Create a boxplot

boxplot(week2.dataset)

#Create a histogram

hist(week2.dataset)

#Create a density plot

plot(density(week2.dataset))

#Recode negative values as NA

tmp.week2.dataset <- week2.dataset #Create a temporary dataset for week2.dataset
tmp.week2.dataset[tmp.week2.dataset < 0] <- NA #Re-code all negative values as missing
sort(tmp.week2.dataset) #Confirm only positive numbers in the dataset
mean(tmp.week2.dataset, na.rm=T) #Calculate the mean without missing values
sd(tmp.week2.dataset, na.rm=T) #Calculate the standard deviation without missing values

#The mean increased from ~34 to ~50
#The standard deviation decreased from ~41 to ~29.

#Log transform the dataset

log(tmp.week2.dataset)

#Create new histograms, boxplots, and mean, median, and standard deviation
#I made these using the log transformed data and removed missing values

tmp1.week2.dataset <- na.omit(tmp.week2.dataset) #Create a temporary dataset to remove all missing values
tmp2.week2.dataset <- log(tmp1.week2.dataset) #Create a temporary dataset to log the dataset that removed all missing values
list(tmp2.week2.dataset) #Check to make sure all missing values are removed and the values are logged  
hist(tmp2.week2.dataset) #Create a histogram for the new dataset
boxplot(tmp2.week2.dataset) #Create a boxplot for the new dataset
plot(density(tmp2.week2.dataset)) #Create a desnity plot without the missing values
mean(tmp2.week2.dataset) #Calculate the mean for the new dataset
median(tmp2.week2.dataset) #Calculate the median for the new dataset
sd(tmp2.week2.dataset) #Calculate the standard deviation for the new dataset