#PC0 Load the dataset

#PC1 Create a two column dataframe with group and lifespan as the column names
lifespan <- c(70, 77, 83, 87, 92, 93, 100, 102, 102, 103, 96, 49, 60, 63, 67, 70, 74, 77, 80, 89, 30, 37, 56, 65, 76, 83, 87, 90, 94, 97, 34, 36, 48, 48, 65, 91, 98, 102)
group <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4 )
df <- data.frame(group, lifespan)

#PC2 Summary statisitics and visuals 

group1ls <- df$lifespan[which(df$group=="1")]
group2ls <- df$lifespan[which(df$group=="2")]
group3ls <- df$lifespan[which(df$group=="3")]
group4ls <- df$lifespan[which(df$group=="4")]

summary(group1ls)
summary(group2ls)
summary(group3ls)
summary(group4ls)

hist(lifespan)
hist(group1ls$lifespan)
hist(group2ls$lifespan)
hist(group3ls$lifespan)
hist(group4ls$lifespan)

mean(df$lifespan)

#PC3 t-test

###Create groups

all <- c(group2ls, group3ls, group4ls)
low <- c(group2ls)
control <- c(group1ls)
high <- c(group4ls)

###t-test for low and all

t.test(low, all)

###t-test for high and control

t.test(high, control)

###t-test in formula notiation

t.test(high, control, data = df)

#PC4 Run an ANOVA

lifespangroup.model <- aov(formula = lifespan ~ group, data = df)
summary(lifespangroup.model)
