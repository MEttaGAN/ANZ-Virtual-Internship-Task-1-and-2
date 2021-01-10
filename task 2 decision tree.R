
################################################################
#  ANZ-InsideSherpa Task 2: Predictive Analysis (decision trees)
# Done by: Megan Ng
################################################################

install.packages("rmarkdown")
##  (1) Load decision tree packages
##  -------------------------------
install.packages("rpart")
install.packages("rpart.plot")
require(rpart)

require(rpart.plot)


##  Load library and data
install.packages('tidyverse')
#remove.packages("mnormt")
#install.packages("mnormt")
#install.packages("psych")
install.packages("dplyr") #tidyverse
library(dplyr)
#library(MASS)
#library(car)
#library(psych)
library(tidyr)
setwd("C:\\Users\\admin\\Desktop\\InsideSherpa (ANZ)\\Task 2\\data")


##  Load Data

data<-read.csv("ANZ synthesised transaction dataset.csv", sep= ';', header = TRUE) #note sep is ';'
#head(data)
#attach(data)
str(data) # gender already in factor , age is integer

###########location############
data_cust_id<-data%>%select(customer_id) #library MASS and car will mask dplyr use without 
data_location<-data%>%separate(col=long_lat,c('longitude', 'latitude'),sep =" ") %>%
  select(longitude,latitude)%>%mutate(latitude= substr(data$long_lat, 9,13))
data_location_numeric<-data.frame<-sapply(data_location,as.numeric)                   
data_id_location<-cbind(data_cust_id,data_location_numeric)
data_location_unique<-data_id_location%>%unique()
#write.csv(data_id_location, "data_location.csv")
#nrow(data_location_unique)                            
##############################                       


data_salary<-data%>% filter(txn_description == "PAY/SALARY") # we r interested only in salary not POS etc
head(data_salary)

data_sales<-data %>% filter(txn_description== 'SALES-POS' | txn_description== 'POS')

data_annual_salary<-data_salary %>% group_by(customer_id) %>% summarise(annual_salary = (sum(amount)/3) *12)

## this gives attributes of spending patterns /habits of customers 
data_var<-data_sales%>% select(customer_id,age,gender,amount,balance)%>%group_by(customer_id)%>%
  mutate(monthly_transaction_volume=n()/3,monthly_transaction_amt=sum(amount)/3,median_balance=median(balance,na.rm=T))%>%
  select(-c("amount","balance"))%>%unique()
final_data<-data_var%>%merge(data_annual_salary)%>%merge(data_location_unique)

final_data$age_group<- ifelse( final_data[,"age"]>=18 & final_data[,"age"]<=30, "young adults",
                               ifelse(final_data[,"age"]>=31 & final_data[,"age"]<=50,"middle-aged adults","older adults"))
head(final_data)
str(final_data) # check all data types are correct before performing regression analysis need to change
# age group to factors later 

data_variables<-final_data%>%select(-c("customer_id"))


final_data$age_group =as.factor(final_data$age_group)





## 2. create training and testing set
##  -------------------------------------
nrow(final_data) #100  80% train 20% test

set.seed(1234) # set seed for reproducibility
index <- sample(100, 80)

train_final_data <- final_data[index,]
test_final_data <- final_data[-index,]
#View(train_final_data)
#View(test_final_data)

## reseting row names (optional)
#rownames(train_final_data) <- NULL
#rownames(test_final_data) <- NULL



##  (3) Grow Decision Tree -- using rpart() command
##  --------------------------------------------------

regression_tree_model <- rpart(annual_salary~age+gender+ monthly_transaction_volume+median_balance+
                                 monthly_transaction_amt+longitude+latitude+age_group,
                               data=train_final_data, method = "anova") ## only training data


##(4) Plot the tree
##  --------------------------------------------------

##  (a) Method 1 -- plot the tree with the prp() function
##  -----------------------------------------------------
#prp(regression_tree_model,main="Regression Tree")


##  (b) Method 2 -- plot the tree with the rpart.plot() function
##  ------------------------------------------------------------
rpart.plot(regression_tree_model,main="Regression Tree") # looks nicer
#rpart.plot(regression_tree_model, cex=0.6) #larger fonts





##  (5) Print the cp table (cp table = complexity parameter table)
##  --------------------------------------------------------------
printcp(regression_tree_model)





## (6)  compute the goodness-of-fit in the TEST set
##  ---------------------------------------------------


pred2 <- predict(regression_tree_model, test_final_data)
#pred2[1:10] # to see the first 10 results of our predictions

##  (b) Compute accuracy of our predictions
##  ----------------------------------------------------
mse <- sum((pred2 - test_final_data$annual_salary)^2)/20  # mse = 669914559 rmse = 25882.71
#  n= size of testing data is 20
mse

var.y <- sum((test_final_data$annual_salary - mean(final_data$annual_salary))^2)/19

rsq <- 1 - mse/var.y

rsq #  0.3478961


##  Part II - Prune the regression Tree 
##  -----------------------------------

##  (1) to prune the tree we use the prune() function
##  --------------------------------------------------
### this function has two main arguments:
### the tree (fit) and the complexity parameter value

### (a) extract the cp value corresponding to
### the lowest cross-validation error (xerror)
##  --------------------------------------------
ocp <- regression_tree_model$cptable[which.min(regression_tree_model$cptable[,"xerror"]),"CP"] 
#we extract the cp of lowest xerror

ocp    #0.07483176

### (b) prune the tree
##  --------------------------------------------
prfit <- prune(regression_tree_model, ocp  )

rpart.plot(prfit) 
## . In this case, we were not able to get a simpler tree based on lowest xerror. 
##    It is essentially the same tree as before.

##Alternatively:
## . If however, we are satisfied with xerror of 1.23797 (instead of 0.91648), then we can prune
##   by extracting the cp value (0.019549)

### (a) extract the cp value corresponding to
### the acceptable cross-validation error (xerror)
##  --------------------------------------------
ocp <- 0.019549


### (b) prune the tree
##  --------------------------------------------
prfit <- prune(regression_tree_model, ocp)

rpart.plot(prfit) 
# we get a simpler tree that is easier to interpret





