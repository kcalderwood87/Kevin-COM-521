#PC1a. Create a crosstab that shows gender and order type 

table1 <- table(lilypad_anonymized$gender, lilypad_anonymized$order_type)
colnames(table1) <- c("Arduino", "Both", "Lilypad")
rownames(table1) <- c("Female", "Male", "Unknown") 
table1

#Create a crosstab that shows gender and order type only for US

US <- subset(lilypad_anonymized, country==81)
table2 <- table(US$gender, US$order_type)
colnames(table2) <- c("Arduino", "Both", "Lilypad")
rownames(table2) <- c("Female", "Male", "Unkown")
table2

#PC1b. Conduct a chi square test and compare to the results in the paper. The results are confirmed. 

chisq.test(table1)
chisq.test(table2)

#PC1c. Install and run the gmodels package. Create an readout similar to SPSS using CrossTable function. 

library(gmodels)
CrossTable(table1)
CrossTable(table2)

#PC1d. Print the tables to a csv using write.csv

write.csv(CrossTable(table1), "table1.csv")
write.csv(CrossTable(table2), "table2.csv")

#PC2

#Create a matrix of Mako and Tommy's group
python <- matrix(c(42, 31, 19, 14), ncol = 2, byrow = TRUE)
colnames(python) <- c("Day 1", "Day 2")
row.names(python) <- c("Mako", "Tommy")
python

#Create a table

python <-as.table(python)

#Run a chi square test and a prop test. In a chi square test for goodness of fit 
#you want a high p value because you do not want significant differences between 
#the observed proportion and the expected proportion. For a chi squared test of independence 
#you would want a low p value. There would be significant differences between group 1 and group 2 with a low p value.
#The results are not statistically significant at the p<.05 level, so there are no significant 
#differences in attrition.

chisq.test(python)
prop.test(python)

#PC3

#Create a table with the obama/not obama and fruit variable
Obama <- table(Halloween2012_2014_2015_PLOS$obama, Halloween2012_2014_2015_PLOS$fruit)
rownames(Obama) <- c("Not Assigned to Obama", "Assigned to Obama")
colnames(Obama) <- c("Did not Choose Fruit", "Chose Fruit")
Obama

#Run a chi square test. The p>.05 means that there is no statistical significance between the groups.  
chisq.test(Obama)

