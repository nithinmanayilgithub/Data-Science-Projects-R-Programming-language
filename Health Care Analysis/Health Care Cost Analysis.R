# Health cost Analysis
# ======================

rm(list=ls())
# To view the present directory 
getwd()

# To view all the libraries installed
library()

#Importing Library for reading Excel file
library("readxl")

# Reading the excel file into hospdat variable
hospdat = read_excel("C://Users//NITHIN//Desktop//github//hospitaldatas.xlsx")

# To view first few rows of data
head(hospdat)

# To get the total number of missing values in each column
colSums(is.na(hospdat))

# Since there is only 1 missing value in the column 'RACE',omit it.
hospdat <- na.omit(hospdat)

# Cross-check if the value is omitted
colSums(is.na(hospdat))

# To view the Dimensions of the data set.
dim(hospdat)

# To view internal structure or summary of the hospital data-set
str(hospdat)

# Here columns 'RACE' and 'FEMALE' contains categorical values.We need to convert them in to factors with respective levels.

# Convert 'RACE' column to a factor(data structure)

hospdat$RACE <- as.factor(hospdat$RACE)

# convert 'FEMALE' column to a factor(data-structure)

hospdat$FEMALE <- as.factor(hospdat$FEMALE)

# cross-check the summary to confirm the change to factors
str(hospdat)

# *** Analysis-1(a):Find The Age category who frequently visit the hospital

summary(as.factor(hospdat$AGE))

# Creating a bar chart of the summary

barplot(summary(as.factor(hospdat$AGE)),xlab = "Age Group",ylab = "Number of visits",col = "blue")

# Conclusion: Age group belonging to zero are the ones who most frequently(306 times) visits the hospital.
 
# *** Analysis-1(b): Find the age category with maximum expenditure

# Importing dplyr library
library(dplyr)

# create a dataframe of column 'AGE' and 'TOTCHG'(aggregate)
df <- summarise(group_by(hospdat,AGE),TOTCHG=sum(TOTCHG))

# Sorting dataframe in ascending order of totalcharges
df <- df[order(df$TOTCHG),]
df

# conclusion: Infant age group(Age = 0) have maximum hospital charges.


# ***Analysis-2 Find the Diagnostic related group with maximum hospitalization and expenditure
APRDGH <- sort(summary(as.factor(hospdat$APRDRG)))

#To view the maximum value
max(APRDGH)
# Diagnostic Group 266 has maximum number of hospitalisations(640)

# create a dataframe of column 'APRDG' and 'TOTCHG'(aggregate)
df1 <- summarise(group_by(hospdat,APRDRG),TOTCHGAPRDG=sum(TOTCHG))
df1

# To Sort In ascending Order
arrange(df1, desc(TOTCHGAPRDG))

# To view only the first row
arrange(df1, desc(TOTCHGAPRDG))[1,]

# Conclusion : Diagnostic group 640 has maximum hospital charges.


# ****Analysis-3: Verify if there is any relationship between race and hospital charges

# To view details of Race
str(hospdat$RACE)

# To view details of Total charges(TOTCHG)
str(hospdat$TOTCHG)

# *****Analysis-4 Perform ANOVA test on TOTCHG(Numerical variable) and Race(categorical variable)
model <- aov(TOTCHG~RACE,data=hospdat)

summary(model)

alpha = 0.05

pvalue = 0.943

pvalue<alpha 

# Here p value is not less than the alpha.so we cannot reject the null hypothesis.i.e,there is no relationship between race and hospital charges.

# ****Analysis-5 Predict length of stay from age,gender and and race

# create a Linear Model (Regression)

Model3 <-lm(LOS~AGE+FEMALE+RACE,data=hospdat)
summary(Model3)

alpha = 0.05

pvalue_age = 0.0818

pvalue_age < alpha

# Conclusion : We cannot reject the null hypothesis.i.e, Length of stage stay is independent of age

pvalue_Female = 0.2586

pvalue_Female < alpha

# Conclusion. we cannot reject the null hypothesis. i.e, there is no correlation between length of stay and gender

pvalue_Race =  0.7883
pvalue_Race < alpha

# Conclusion. we cannot reject the null hypothesis. i.e, there is no correlation betwwen length of stay and race

#****Analysis-6. Find the variable that mainly affects the hospital costs.

model_comp <- lm(TOTCHG~.,hospdat)
summary(model_comp)

# AGE, LOS and APRDG have three stars in the summary. so these are the variables that are significant.These are the variables that influence hospital costs
