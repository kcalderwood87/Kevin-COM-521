###PC0 Load the population dataset

population <- read.delim("GitHub/uwcom521-assignments/week_05/com521_population.tsv", sep="\t", header=TRUE)
populationmeanx <- mean(population$x)

###PC1a Calculate the 95% confidence interval for variable x by hand

week3_dataset_kevin
samplesize <- nrow(week3_dataset_kevin)
meanx <- mean(week3_dataset_kevin$x)
sdx <- sd(week3_dataset_kevin$x)
standarderrorx <- sdx/sqrt(samplesize)
right95 <- meanx+1.96*standarderrorx
left95 <- meanx-1.96*standarderrorx


###PC1b Calculate the 95% confidence interval for variable x with t.test function

t.test(week3_dataset_kevin$x)


###PC1c Compare the mean from your sample and the CI to the true population mean

left95 < populationmeanx
right95 > populationmeanx

###PC2

#Calculate summary statistics and a histogram for sample x

summary(week3_dataset_kevin$x)
hist(week3_dataset_kevin$x)

#Calculate summary statistics for population x

summary(population$x)
hist(population$x)

###PC3

#Calculate the mean for population y

populationmeany <- mean(population$y)

#Calculate the 95% confidence interval for y. Is population mean in or out? Yes, it is within the confidence interval.

t.test(week3_dataset_kevin$y)
populationmeany

###PC4a Create a vector of 10,000 randomly generated numbers from 0-9

Random <- runif(10000, 0, 9)

###PC4b Take the mean and draw a histogram of the randomly generated vector

mean(Random)
hist(Random)

###PC4c-d Take a random sample, calculate the means, and draw a histogram 

Random100 <- replicate(100, sample(Random, 2))
Random100means <- colMeans(Random100)
hist(Random100means)

Random100a <- replicate(100, sample(Random, 10))
Random100ameans <- colMeans(Random100a)
hist(Random100ameans)

Random100b <- replicate(100, sample(Random, 100))
Random100bmeans <- colMeans(Random100b)
hist(Random100bmeans)

###PC5 Repeat PC4 using a normal distribution 

Randomnormal <- rnorm(10000, mean = 42, sd = 42)
mean(Randomnormal)
hist(Randomnormal)

Randomnormal100 <- replicate(100, sample(Randomnormal, 2))
Randomnormal100means <- colMeans(Randomnormal100)

Randomnormal100a <- replicate(100, sample(Randomnormal, 10))
Randomnormal100ameans <- colMeans(Randomnormal100a)
hist(Randomnormal100ameans)

Randomnormal100b <- replicate(100, sample(Randomnormal, 100))
Randomnormal100bmeans <- colMeans(Randomnormal100b)
hist(Randomnormal100bmeans)
