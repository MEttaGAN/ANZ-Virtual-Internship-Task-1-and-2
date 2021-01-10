
##  ==============================================================
##    Data Wrangling using dplyr and Data Cleaning
##  
##  ==============================================================

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

nrow(data_salary)#883
nrow(data) # 12043 dataset,  r auto exclude counting header 


## testing out data to see how many/frequency of pay/salary records per customer since possible to vary 

cust_1_data<-data_salary %>% filter(customer_id == "CUS-1462656821") # this cust id only has 7 salary records for over 3 mths
cust_1_data

cust_2_data<-data_salary %>% filter(customer_id=="CUS-2500783281") # 14 salary records over 3 mths
cust_2_data
cust_3_data<-data_salary %>% filter(customer_id=="CUS-326006476") #14 salary records over 3 mths
cust_3_data
cust_4_data<-data_salary%>% filter(customer_id=="CUS-1140341822")
cust_4_data
# here we conclude to find annual salary of each customer, we have to find their respective average salary over 3mths x 12 = annual salary

unique_id<-unique(data_salary$customer_id)
df_unique_id<-data.frame(customer_id= unique_id)
#df_unique_id

#df_unique_id$customer_id[1] # first row aka first customer ID 



## records only span across 3 mths august,sep,oct , so annual salary = sum(amount)/3 *12

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


############################################################################################

## Data exploration

##scatterplot matrix ##

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


pairs(annual_salary~age+gender+monthly_transaction_volume+monthly_transaction_amt+median_balance+longitude+latitude+age_group, data=final_data,
      lower.panel=panel.cor, upper.panel=panel.smooth, 
      pch=20, main=" Scatterplot Matrix")

x<-cbind(annual_salary,age,gender,monthly_transaction_volume,monthly_transaction_amt,median_balance,longitude,latitude,age_group)

cor(x)




##  ===================================
##  (A) Model Building
##  ===================================      
final_data$age_group =as.factor(final_data$age_group)


##model 1
model_1<-lm(annual_salary~.-customer_id,data=final_data)
summary(model_1)
anova(model_1)

#model_step<-MASS::stepAIC(model_1)summary(model_2)
step_model_1<-step(model_1,direction=c("both"))
summary(step_model_1)
anova(step_model_1)

################ box cox transformation ################
MASS::boxcox(step_model_1, lambda=seq(-4, 4, by=0.5),optimize=TRUE,plotit = TRUE) #lambda =0 

model_2<-lm(annual_salary~age+median_balance+longitude+latitude+age_group,data=final_data)
summary(model_2)

step_model_2<-step(model_2,direction=c("both")) # model_2 and step_model_2 both propose same 
summary(step_model_2)

## conclusion model_2 best model 

##  =================================            
##  (B) Model Assessment
##  =================================

#######################
## constant variance ?
#######################
plot(step_model_1$fitted.values,rstandard(step_model_1), xlab="fitted", ylab= "Standardized Residuals", main = " Step Model 1",pch=20)
abline(h=0)

plot(model_2$fitted.values,rstandard(model_2), xlab="fitted", ylab= "Standardized Residuals", main = " Model 2",pch=20)
abline(h=0)

plot(step_model_2$fitted.values,rstandard(step_model_2), xlab="fitted", ylab= "Standardized Residuals", main = " Step Model 2",pch=20)
abline(h=0)

#########################
## Normality assumption
#########################
#Normal probability plot =  QQ plot
qqnorm(rstandard(step_model_1),datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", main = "Normal Probability Plot Step Model 1",pch=20)
qqline(rstandard(step_model_1),datax = TRUE)

qqnorm(rstandard(step_model_2),datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", main = "Normal Probability Plot Step Model 2",pch=20)
qqline(rstandard(step_model_2),datax = TRUE)

########################
## mulit-collinearity
#######################

car:: vif(step_model_1) # VIF <10 

car::vif(step_model_2)









##  (c) Prediction Errors (= Residual Standard Error / mean) --> coefficient of variation
##  ---------------------------------------------------------------------------------------
predictionError_stepmodel_1 <- sigma(step_model_1)/mean(final_data$annual_salary)
predictionError_stepmodel_1     #0.3871727
# note: here, RSE = sigma(model_1)

predictionError_stepmodel_2 <- sigma(step_model_2)/mean(final_data$annual_salary)
predictionError_stepmodel_2          #5.477038e-06  

##  .Step Model 2 is better (as it has lower error rate)
##            The lower the better --> model 2 better.



## Alternatively if you wish to use mse (mean square error)
##  --------------------------------------------------------
mse_stepmodel_1 <- mean(step_model_1$residuals^2)  
mse_stepmodel_1  #626988201


mse_stepmodel_2 <- mean(step_model_2$residuals^2)
mse_stepmodel_2 #0.1254704 


##  . In this case, if we are to use mse, we will select step model 2 as it 
##    provides a lower error rate.



## More alternatives:

## RMSE
RMSE <- function(error) { sqrt(mean(error^2)) }  ## Function for Root Mean Squared Error
RMSE(step_model_1$residuals) #25039.73
RMSE(step_model_2$residuals)  #0.354218




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




##  3.  train our model using training data
##  ---------------------------------------
trained_model <- lm(log(annual_salary)~age+median_balance+longitude+latitude+age_group,data=train_final_data)

# 4.  run the trained model on testing data
##  ------------------------------------------
pred.test <- predict(trained_model, newdata = test_final_data)

length(pred.test)

head(pred.test)


##(5) display result in an additional column in a dataframe
##  ------------------------------------------------------
result_test <- data.frame(
  actual_annual_salary = test_final_data$annual_salary,
  predicted_annual_salary = pred.test
)

mse_function <- function (x) {
  mean((as.numeric(x[1]) - as.numeric(x[2]))^2)
}


result_test$mse_column <- apply(result_test[,c("actual_annual_salary", "predicted_annual_salary")], 1, mse_function)
View(result_test)


mse <- mean(result_test$mse_column)
mse   

