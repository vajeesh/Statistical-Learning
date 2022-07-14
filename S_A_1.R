library(writexl)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(MASS)
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

#import the edited excel file in R as dataframe 
US_data = read_excel("C:/Users/Maha Vajeeshwaran/Desktop/STATISTICAL LEARNING/ASSIGNMENT 1/ANES2016.xlsx") 
#C:\Users\Maha Vajeeshwaran\Desktop\STATISTICAL LEARNING\ASSIGNMENT 1

US_data <- na.omit(US_data)  # removes na values from the data

#a)  delete  -8 and -9 which doesn't have much information

US_data <- US_data[US_data$Media >= 0,]
US_data <- US_data[US_data$FamSize >= 0,]
US_data <- US_data[US_data$Hillary >= 0,]
US_data <- US_data[US_data$Trump >= 0,]
US_data <- US_data[US_data$Age >= 0,]
US_data <- US_data[US_data$Education >= 0,]
US_data <- US_data[US_data$Employment >= 0,]
US_data <- US_data[US_data$Birthplace >= 0,]
US_data <- US_data[US_data$GBirth >= 0,]
US_data <- US_data[US_data$Dependent >= 0,]
US_data <- US_data[US_data$Housing >= 0,]
US_data <- US_data[US_data$Income >= 0,]
US_data <- US_data[US_data$Partner >= -1,]
US_data <- US_data[US_data$SpouseEdu >= -1,]

nrow(US_data)
set.seed(7)
# creating correlation matrix
corr_mat <- round(cor(US_data),2)

# reduce the size of correlation matrix
install.packages("reshape2")
library(reshape2)
melted_corr_mat <- melt(corr_mat)
#  correlation heatmap plotting
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile()
View(corr_mat)
# it is found that interaction between some of the features
# to to find the clear relationship I am comparing the relationship with sing feature with response.

#a) Recode trump variable, delete NA-values (NA-Values are -8 and -9)

data_t <- US_data[US_data$Trump >= 0,]
data_t$Trump <- ifelse(data_t$Trump <=3, "Liberal", "Conservative")
data_t$Trump <- as.factor(data_t$Trump)
table(data_t$Trump)

#I fitted a logistic regression to see how the what the coefficients and p-Values are for each attribute.


data_t$Trump <- as.factor(data_t$Trump)
table(data_t$Trump)
glm.fit <- glm(Trump~Media+FamSize+Hillary+Age+Education+Employment+
                 Birthplace+GBirth+Dependent+Housing+Income+Partner+
                 SpouseEdu,"binomial", data=data_t)
summary(glm.fit)$coefficients
summary(glm.fit)

#Above code test with all the variables with multicolinearity 
# I check the relationship with single variable below.


t_media <- subset(data_t, Media>=0) # Remove NA
t_media$Trump <- as.factor(t_media$Trump)
glm.fit <- glm(Trump~Media,"binomial", data=t_media)
summary(glm.fit)
# Meedia has strong relationship with Trump

t_FamSize <- subset(data_t, FamSize>=0) # Remove NA
t_FamSize$Trump <- as.factor(t_FamSize$Trump)
glm.fit <- glm(Trump~FamSize,"binomial", data=t_FamSize)
summary(glm.fit)
## Famsize has low significant 

t_Hillary <- subset(data_t, Hillary>=0) # Remove NA
t_Hillary$Trump <- as.factor(t_Hillary$Trump)
glm.fit <- glm(Trump~Hillary,"binomial", data=t_Hillary)
summary(glm.fit)
## Hillary is more significant

t_Age <- subset(data_t, Age>=0) # Remove NA
t_Age$Trump <- as.factor(t_Age$Trump)
glm.fit <- glm(Trump~Age,"binomial", data=t_Age)
summary(glm.fit)
# Age has very low significant

t_Education <- subset(data_t, Education>=0) # Remove NA
t_Education$Trump <- as.factor(t_Education$Trump)
glm.fit <- glm(Trump~Education,"binomial", data=t_Education)
summary(glm.fit)
# Education has very high significant

t_Employment <- subset(data_t,Employment>=0) # Remove NA
t_Employment$Trump <- as.factor(t_Employment$Trump)
glm.fit <- glm(Trump~Employment,"binomial", data=t_Employment)
summary(glm.fit)
## Employment has low significant

t_Birthplace <- subset(data_t,Birthplace>=0) # Remove NA
t_Birthplace$Trump <- as.factor(t_Birthplace$Trump)
glm.fit <- glm(Trump~Birthplace,"binomial", data=t_Birthplace)
summary(glm.fit)
## Birthplace has very high significant

t_GBirth <- subset(data_t,GBirth>=0) # Remove NA
t_GBirth$Trump <- as.factor(t_GBirth$Trump)
glm.fit <- glm(Trump~GBirth,"binomial", data=t_GBirth)
summary(glm.fit)
## GBirth has very low significant

t_Dependent <- subset(data_t,Dependent>=0) # Remove NA
t_Dependent$Trump <- as.factor(t_Dependent$Trump)
glm.fit <- glm(Trump~Dependent,"binomial", data=t_Dependent)
summary(glm.fit)
## Dependent has very high significant

t_Housing <- subset(data_t,Housing>=0) # Remove NA
t_Housing$Trump <- as.factor(t_Housing$Trump)
glm.fit <- glm(Trump~Housing,"binomial", data=t_Housing)
summary(glm.fit)
## Housing has very high significant

t_Income <- subset(data_t,Income>=0) # Remove NA
t_Income$Trump <- as.factor(t_Income$Trump)
glm.fit <- glm(Trump~Income,"binomial", data=t_Income)
summary(glm.fit)
## Income has very high significant

t_Partner <- subset(data_t,Partner>=-1) # Remove NA
t_Partner$Trump <- as.factor(t_Partner$Trump)
glm.fit <- glm(Trump~Partner,"binomial", data=t_Partner)
summary(glm.fit)
## Partner has very high significant

t_SpouseEdu <- subset(data_t,SpouseEdu>=-1) # Remove NA
t_SpouseEdu$Trump <- as.factor(t_SpouseEdu$Trump)
table(t_SpouseEdu$Trump)
glm.fit <- glm(Trump~SpouseEdu,"binomial", data=t_SpouseEdu)
summary(glm.fit)
## SpouseEdu has very high significant


