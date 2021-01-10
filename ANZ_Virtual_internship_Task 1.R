##=======================================================
## Task 1 : Explanatory Data Analysis
## what is the average transaction amount? How many transactions do customers make each month, on average?
## Segment the dataset by transaction date and time. Visualise transaction volume and spending over the course of an
## average day or week. Consider the effect of any outliers that may distort your analysis.
##
## Insidesherpa : Data@ANZ virtual internship
## Done by: Megan Ng 
##=======================================================



##  ==============================================================
##    Data Wrangling using dplyr and Data Cleaning
##  
##  ==============================================================

##  Load library and data


install.packages("dplyr") #tidyverse
install.packages("tidyverse")
# Alternatively, install just tidyr:
install.packages("tidyr")
library(dplyr)
library(tidyr)
library(lubridate)
setwd("C:\\Users\\admin\\Desktop\\InsideSherpa (ANZ)\\Task 1\\data")

##  Load Data

data<-read.csv("ANZ synthesised transaction dataset.csv", sep= ';', header = TRUE) #note sep is ';'
#head(data)
attach(data)



## Data preparation 
str(data)
summary(data)

## change date format and wrangle dates for easier segmentation and analysis later
data$date<-as.Date(data$date, "%d/%m/%Y")
#head(data)

amended_data <-separate(data, 
                        col = extraction, #column name containing the data variables
                        into = c('date', 'time'),
                        sep = 10, #split after the 8th character (05/16/17 10:34AM); sep = " ",
                        remove = T)
#head(amended_data)  split the extraction data into date and time of transaction 

time<-as.POSIXct(substr(amended_data$time,2,8),format="%H:%M:%S")
hour <-hour(time)
minute <-format(as.POSIXct(time,format="%H:%M:%S"),"%M")
amended_data$day <- weekdays(as.Date(amended_data$date))
amended_data$hours<-hour
#amended_data$month <-format(as.POSIXct(amended_data$date,'%Y:%m:%d'),"%m") # month column 

#head(amended_data)
#tail(amended_data)
#head(data)



## prepare data for segmenting by age group later
## young adults (ages 18-35 years), middle-aged adults (ages 36-55 years), and older adults (aged older than 55 years)

min(amended_data[,"age"]) # youngest age is 18 
max(amended_data[,"age"]) #oldest is 78
#hist(amended_data$age, xlab="age", main="Distribution of age among population")
#summary(amended_data$age)
amended_data$age_group<- ifelse( amended_data[,"age"]>=18 & amended_data[,"age"]<=35, "young adults",
                                 ifelse(amended_data[,"age"]>=36 & amended_data[,"age"]<=55,"middle-aged adults","older adults"))
                                                                                    

## checking missing or error in data ? 
#complete.cases(amended_data[,c("amount","date","txn_description","customer_id")]) 

#complete.cases(amended_data[,"amount"])
#which(complete.cases(amended_data[,"amount"]))

#which(complete.cases(amended_data[,c("amount","date","txn_description","customer_id")]))
length(unique(amended_data$customer_id)) #100 str(data)
length(unique(amended_data$date)) #only 91 but data supposed from data 92 


##=================================================================
## How many transactions do customers make each month, on average?
## ===============================================================
par(mfrow=c(2,2))
  
#df_test<-amended_data %>% group_by(customer_id) %>% summarise(num=n())
#df_test
  
## As dataset is over 3 mths period, hence transaction vol div by 3.Each customer_id record corresponds to one transaction
df_mth_vol<-amended_data %>% group_by(customer_id) %>% summarise(mthly_transaction_volume = n()/3) #  over 3 mths data
#df_mth_vol
#summary(df_mth_vol$mthly_transaction_volume) #mean is 40.143
hist(df_mth_vol$mthly_transaction_volume, xlab="Monthly Transaction (Volume)", ylab= "Number of customers",col="yellow", main="Histogram of transaction (volume)")
abline(v=mean(df_mth_vol$mthly_transaction_volume),col="red")

##==============================================
## what is the average transaction amount?
##==============================================
df_avg_amt<-amended_data %>% group_by(customer_id) %>% summarise(transaction_amount = sum(amount))
#df_avg_amt
#summary(df_avg_amt$transaction_amount) #mean 2263
hist(df_avg_amt$transaction_amount, xlab="Transaction (Amount)", ylab = "Number of customers",col="yellow",main="Histogram of transaction amount")
abline(v=mean(df_avg_amt$transaction_amount),col="red") # this lines marks the average :2263
 #boxplot(amended_data$amount)



##===================================================================================================
## Segment the dataset by transaction date and time. Visualise transaction volume and spending over
## the course of an average day or week.
##====================================================================================================
# this gives the number of weeks to average over :
#df_week<-amended_data%>%select(date,day)%>% group_by(date,day)%>%summarise(num=n())%>%group_by(day)%>%summarise(n=n())
# this gives the daily transaction volume
#df_week<-amended_data%>%select(date,day)%>% group_by(date,day)%>%summarise(num=n())

##=====================================================
## Visualise transaction volume over the course of a week
##======================================================
df_week<-amended_data%>% group_by(date,day)%>%summarise(number_transaction_day=n())%>%
  group_by(day)%>%summarise(volume_weekly=mean(number_transaction_day))
df_week



### change weekday to factors and sort 
df_week$day<-factor(df_week$day,levels=c("Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))


df_week_ordered<-df_week[order(df_week$day),]

### line plot

#v_week<-c(df_week_ordered$volume_weekly)
#plot(v_week, type="o",xlab="day of the week",ylab="transaction volume",main="Transaction volume over a week")

## bar plot
name<-c("Mon", 
        "Tue", "Wed", "Thurs", "Fri", "Sat","Sun")

barplot(v_week,names.arg=name,xlab="day of the week",ylab="transaction volume",main="Transaction volume over a week")



##=====================================================
## Visualise transaction vol over the course of a day
##======================================================


df_a_day<-amended_data%>%group_by(date,hours)%>%summarise(number_transaction_hour=n())%>%
  group_by(hours)%>%summarise(volume_daily=mean(number_transaction_hour))



### line plot

v_day<-c(df_a_day$volume_daily)
plot(v_day, type="o",xlab="hours",ylab="transaction volume",main="Transaction volume over a day")


##===============================================
## composition for each type of transaction (6 types)
##=====================================================


pos<-amended_data %>% filter(txn_description == "POS")
#nrow(pos)
pos_sales<-amended_data %>% filter(txn_description == "SALES-POS")
payment<-amended_data %>% filter(txn_description == "PAYMENT")
interbank<-amended_data %>% filter(txn_description == "INTER BANK")
pay_salary<-amended_data %>% filter(txn_description == "PAY/SALARY")
phone_bank<-amended_data %>% filter(txn_description == "PHONE BANK")

slices <- c(nrow(pos),nrow(pos_sales), nrow(payment),nrow(interbank),nrow(pay_salary),nrow(phone_bank))
label_names <- c("POS", "POS_SALES", "PAYMENT","INTERBANK", 'SALARY', 'PHONEBANK')
pct <- round(slices/sum(slices)*100)
label_names <- paste(label_names, pct) # add percents to labels
label_names <- paste(label_names, "%", sep="") # add % to labels
pie(slices, 
    labels=label_names, 
    # col= rainbow(length(label_names)),
    # col=heat.colors(length(label_names)),
    # col=terrain.colors(length(label_names)),
    col=c("green", "lightblue", "gold","red","purple","grey"),
    main="Composition of transaction type")

# since composition of POS 31% and POS_SALES 33% highest will look into them 


## filter POS and sales-pos to study spending patterns
## as contain other transactions 
## -what is the average spending volume and spending amount?
## -spending pattern between male and female
## -spending pattern among different age groups 
##=====================================================

nrow(amended_data[txn_description== 'SALES-POS' | txn_description== 'POS',]) #checking r/s btw merchant id and sales-pos
nrow(amended_data[merchant_id != " ",]) #merchant id correspond to purchase
     
df_sales<-amended_data %>% filter(txn_description== 'SALES-POS' | txn_description== 'POS')
boxplot(df_sales$amount, main="Transaction amount of sales(purchase)")
outliers_sales_amt<-boxplot.stats(df_sales$amount)$out 
#length(outliers_sales_amt)  685 outliers out of 7717
df_sales_no_outliers<-df_sales %>% filter(!df_sales$amount %in% outliers_sales_amt)

##=================================================================================
## what is the average transaction amount specifically purchases(aka POS and sales)
##==================================================================================
df_avg_amt_sales<-df_sales_no_outliers %>% group_by(customer_id) %>% summarise(transaction_amount_sales = sum(amount))
hist(df_avg_amt_sales$transaction_amount_sales,xlab="Transaction amount of sales(purchase)",ylab="Number of customers(merchants)"
     ,main="Histogram of transaction amount of sales(purchase)",col="yellow")

abline(v=mean(df_avg_amt_sales$transaction_amount_sales),col="red")
summary(df_avg_amt_sales$transaction_amount_sales) # mean 1530.5 
##======================================================================
## How many sales(purchase) transactions do customers make each month, on average?
##======================================================================

df_mth_sales_vol<-df_sales_no_outliers%>% group_by(customer_id) %>% summarise(mthly_salestransaction_volume = n()/3) #  over 3 mths data

hist(df_mth_sales_vol$mthly_salestransaction_volume, xlab="Monthly Sales(purchase) Transaction Volume ", ylab= "Number of customers(merchants)",
     col="yellow", main="Histogram of Sales(purchase) transaction volume")
abline(v=mean(df_mth_sales_vol$mthly_salestransaction_volume),col="red")
summary(df_mth_sales_vol$mthly_salestransaction_volume) # 23.44
##===================================================================================================
## what is the average transaction amount specifically purchases(aka POS and sales) for each age group
##====================================================================================================
par(mfrow=c(2,2)) # to combine plots 

## young adults
df_avg_amt_young<-df_sales_no_outliers %>% filter(age_group == "young adults")%>%
                  group_by(customer_id)%>%summarise(transaction_amount_sales_young = sum(amount))
#summary(df_avg_amt_young$transaction_amount_sales_young) #mean 1609.5

hist(df_avg_amt_young$transaction_amount_sales_young, xlab="Transaction amount of purchase (Young)",ylab="Number of Young customers(merchants)"
     ,main="Histogram of transaction amount of purchase (Young)",col="yellow")
abline(v=mean(df_avg_amt_young$transaction_amount_sales_young),col="red")

##middle aged adults

df_avg_amt_middle<-df_sales_no_outliers %>% filter(age_group == "middle-aged adults")%>%
  group_by(customer_id)%>%summarise(transaction_amount_sales_middle = sum(amount))

#summary(df_avg_amt_middle$transaction_amount_sales_middle) #mean 1424

hist(df_avg_amt_middle$transaction_amount_sales_middle, xlab="Transaction amount of purchase (Middle)",ylab="Number of Middle-aged customers(merchants)"
     ,main="Histogram of transaction amount of purchase (Middle-aged)",col="yellow")
abline(v=mean(df_avg_amt_middle$transaction_amount_sales_middle),col="red")

## older adults

df_avg_amt_old<-df_sales_no_outliers %>% filter(age_group == "older adults")%>%
  group_by(customer_id)%>%summarise(transaction_amount_sales_old = sum(amount))

hist(df_avg_amt_old$transaction_amount_sales_old, xlab="Transaction amount of purchase (Old)",ylab="Number of older customers(merchants)"
     ,main="Histogram of transaction amount of purchase (Older adults)",col="yellow")
abline(v=mean(df_avg_amt_old$transaction_amount_sales_old),col="red")



## pie charts of transaction amt by age groups 
df_young<-df_sales_no_outliers %>% filter(age_group == "young adults")%>%summarise(amount_young = sum(amount))
young_count<-df_young$amount_young #104614.8
#young_count

df_middle<-df_sales_no_outliers %>% filter(age_group == "middle-aged adults")%>%summarise(amount_middle = sum(amount)) 
middle_aged_count<-df_middle$amount_middle
#middle_aged_count
  
df_old<-df_sales_no_outliers %>% filter(age_group == "older adults")%>%summarise(amount_old = sum(amount)) 
older_count<-df_old$amount_old
#older_count

#values<-c(young_count, middle_aged_count, older_count)
#barplot(values)

slices <- c(young_count, middle_aged_count, older_count)
label_names <- c("Young adults", "Middle-aged adults", "Older adults")
pct <- round(slices/sum(slices)*100)
label_names <- paste(label_names, pct) # add percents to labels
label_names <- paste(label_names, "%", sep="") # add % to labels
pie(slices, 
    labels=label_names, 
    # col= rainbow(length(label_names)),
    # col=heat.colors(length(label_names)),
    # col=terrain.colors(length(label_names)),
    col=c("green", "lightblue", "gold"),
    main="Pie Chart of Transaction Amount (purchase) by aged group")

##pie chart of composition of each age group of the merchants 
old<-nrow(df_sales_no_outliers %>% filter(age_group == "older adults")%>%
  group_by(customer_id)%>%summarise(transaction_amount_sales_old = sum(amount)))

middle<-nrow(df_sales_no_outliers %>% filter(age_group == "middle-aged adults")%>%
                       group_by(customer_id)%>%summarise(transaction_amount_sales_middle = sum(amount)))
young<-nrow(df_sales_no_outliers %>% filter(age_group == "young adults")%>%
                        group_by(customer_id)%>%summarise(transaction_amount_sales_young = sum(amount)))


slices <- c(young, middle, old)
label_names <- c("Young adults", "Middle-aged adults", "Older adults")
pct <- round(slices/sum(slices)*100)
label_names <- paste(label_names, pct) # add percents to labels
label_names <- paste(label_names, "%", sep="") # add % to labels
pie(slices, 
    labels=label_names, 
    # col= rainbow(length(label_names)),
    # col=heat.colors(length(label_names)),
    # col=terrain.colors(length(label_names)),
    col=c("green", "lightblue", "gold"),
    main="Composition of merchants from each aged group")            



##===================================================================================================
## what is the average transaction amount specifically purchases(aka POS and sales) for different genders?
##====================================================================================================
par(mfrow=c(2,2)) # to combine plots 

## Male
df_avg_amt_male<-df_sales_no_outliers %>% filter(gender == "M")%>%
  group_by(customer_id)%>%summarise(transaction_amount_sales_male = sum(amount))

summary(df_avg_amt_male$transaction_amount_sales_male) # mean 1371.81
hist(df_avg_amt_male$transaction_amount_sales_male, xlab="Transaction amount of purchase (Male)",ylab="Number of Male customers(merchants)"
     ,main="Histogram of transaction amount of purchase (Male)",col="yellow",xlim=c(0,7000))
abline(v=mean(df_avg_amt_male$transaction_amount_sales_male),col="red")

## Female

df_avg_amt_female<-df_sales_no_outliers %>% filter(gender == "F")%>%
  group_by(customer_id)%>%summarise(transaction_amount_sales_female = sum(amount))
summary(df_avg_amt_female$transaction_amount_sales_female) #mean 1732.5

hist(df_avg_amt_female$transaction_amount_sales_female, xlab="Transaction amount of purchase (Female)",ylab="Number of Female customers(merchants)"
     ,main="Histogram of transaction amount of purchase (Female)",col="yellow")
abline(v=mean(df_avg_amt_female$transaction_amount_sales_female),col="red")


## pie chart total amount of transaction by female and male 
df_male<-df_sales_no_outliers %>% filter(gender == "M")%>%summarise(amount_sales_male = sum(amount))
amount_male<-df_male$amount_sales_male
amount_male

df_female<-df_sales_no_outliers %>% filter(gender == "F")%>%summarise(amount_sales_female = sum(amount))
amount_female<-df_female$amount_sales_female 
amount_female

slices <- c(amount_male, amount_female)
label_names <- c("Male", "Female")
pct <- round(slices/sum(slices)*100)
label_names <- paste(label_names, pct) # add percents to labels
label_names <- paste(label_names, "%", sep="") # add % to labels
pie(slices, 
    labels=label_names, 
    # col= rainbow(length(label_names)),
    # col=heat.colors(length(label_names)),
    # col=terrain.colors(length(label_names)),
    col=c("green", "lightblue"),
    main="Composition of transaction amount(purchase) by gender") 

## composition of male to female 
male<-nrow(df_sales_no_outliers %>% filter(gender == "M")%>%
               group_by(customer_id)%>%summarise(transaction_amount_sales_male = sum(amount)))
female<-nrow(df_sales_no_outliers %>% filter(gender == "F")%>%
              group_by(customer_id)%>%summarise(transaction_amount_sales_female = sum(amount)))


slices <- c(male, female)
label_names <- c("Male", "Female")
pct <- round(slices/sum(slices)*100)
label_names <- paste(label_names, pct) # add percents to labels
label_names <- paste(label_names, "%", sep="") # add % to labels
pie(slices, 
    labels=label_names, 
    # col= rainbow(length(label_names)),
    # col=heat.colors(length(label_names)),
    # col=terrain.colors(length(label_names)),
    col=c("green", "lightblue"),
    main="Composition of Male and Female") 

par(mfrow=c(1,1)) # reset 



##===================================================================================================
## gender and age group combined analysis ? : bar plot filter age and gender for purchase amount
## and purchase transaction volume
##====================================================================================================
par(mfrow=c(1,2))

##transaction volume 
df_young_male_vol<-df_sales_no_outliers %>% filter(age_group == "young adults"& gender == "M")%>%summarise(volume_young_male = n())
young__male_volume<-df_young_male_vol$volume_young_male


df_young_female_vol<-df_sales_no_outliers %>% filter(age_group == "young adults"& gender == "F")%>%summarise(volume_young_female = n())
young__female_volume<-df_young_female_vol$volume_young_female 

df_middle_male_vol<-df_sales_no_outliers %>% filter(age_group == "middle-aged adults"& gender == "M")%>%summarise(volume_middle_male = n()) 
middle_aged__male_volume<-df_middle_male_vol$volume_middle_male

df_middle_female_vol<-df_sales_no_outliers %>% filter(age_group == "middle-aged adults"& gender == "F")%>%summarise(volume_middle_female = n()) 
middle_aged__female_volume<-df_middle_female_vol$volume_middle_female 

df_old_male_vol<-df_sales_no_outliers %>% filter(age_group == "older adults"& gender == "M")%>%summarise(volume_old_male = n()) 
older_male_volume<-df_old_male_vol$volume_old_male 

df_old_female_vol<-df_sales_no_outliers %>% filter(age_group == "older adults"& gender == "F")%>%summarise(volume_old_female = n()) 
older_female_volume<-df_old_female_vol$volume_old_female 

values_age_gender_vol<-c(young__male_volume,young__female_volume,middle_aged__male_volume,middle_aged__female_volume
                         ,older_male_volume,older_female_volume)
data_a_g_volume<-matrix(values_age_gender_vol, nrow=2)

rownames(data_a_g_volume)<-c("Male","Female")
colnames(data_a_g_volume)<-c("Young","Middle-aged","Old")
barplot(data_a_g_volume,beside=T,legend=rownames(data_a_g),col=colors()[c(65,60)],xlab="Age Group",ylab="Transaction Volume(purchase)",
        main="Transaction volume (purchase) over 3 months period ")


##transaction amount

df_young_male<-df_sales_no_outliers %>% filter(age_group == "young adults"& gender == "M")%>%summarise(amount_young_male = sum(amount))
young__male_count<-df_young_male$amount_young_male #53106.6

df_young_female<-df_sales_no_outliers %>% filter(age_group == "young adults"& gender == "F")%>%summarise(amount_young_female = sum(amount))
young__female_count<-df_young_female$amount_young_female #51508.2

df_middle_male<-df_sales_no_outliers %>% filter(age_group == "middle-aged adults"& gender == "M")%>%summarise(amount_middle_male = sum(amount)) 
middle_aged__male_count<-df_middle_male$amount_middle_male #22189.17

df_middle_female<-df_sales_no_outliers %>% filter(age_group == "middle-aged adults"& gender == "F")%>%summarise(amount_middle_female = sum(amount)) 
middle_aged__female_count<-df_middle_female$amount_middle_female #23377.45

df_old_male<-df_sales_no_outliers %>% filter(age_group == "older adults"& gender == "M")%>%summarise(amount_old_male = sum(amount)) 
older_male_count<-df_old_male$amount_old_male #1525.75

df_old_female<-df_sales_no_outliers %>% filter(age_group == "older adults"& gender == "F")%>%summarise(amount_old_female = sum(amount)) 
older_female_count<-df_old_female$amount_old_female # 1343.72

values_age_gender<-c(young__male_count,young__female_count,middle_aged__male_count,middle_aged__female_count,older_male_count,older_female_count)
data_a_g<-matrix(values_age_gender, nrow=2)

rownames(data_a_g)<-c("Male","Female")
colnames(data_a_g)<-c("Young","Middle-aged","Old")
barplot(data_a_g,beside=T,legend=rownames(data_a_g),col=colors()[c(89,12)],xlab="Age Group",ylab="Transaction Amount(purchase)",
        main="Transaction amount (purchase) over 3 months period ")

##==============================================================================
## visualise transaction volume (purchase) of aged group over a week and day
##=============================================================================

################### over a week ###########################


##===========
##young
##===========

df_purchasevolume_overaweek_young<-df_sales_no_outliers %>% filter(age_group == "young adults") %>%group_by(date,day)%>%
  summarise(number_transaction_day=n())%>%group_by(day)%>%summarise(volume_weekly=mean(number_transaction_day))


### change weekday to factors and sort 
df_purchasevolume_overaweek_young$day<-factor(df_purchasevolume_overaweek_young$day,levels=c("Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

df_purchasevolume_overaweek_young_ordered<-df_purchasevolume_overaweek_young[order(df_purchasevolume_overaweek_young$day),]

### line plot

val_young<-c(df_purchasevolume_overaweek_young_ordered$volume_weekly)
plot(val_young, type="o",col="red",xlab="day of the week",ylab="transaction volume (purchase) of young adults",main=" Purchase transaction volume over a week of young adults")

##===========
## middle-aged
##===========

df_purchasevolume_overaweek_middle<-df_sales_no_outliers %>% filter(age_group == "middle-aged adults") %>%group_by(date,day)%>%
  summarise(number_transaction_day=n())%>%group_by(day)%>%summarise(volume_weekly=mean(number_transaction_day))


### change weekday to factors and sort 
df_purchasevolume_overaweek_middle$day<-factor(df_purchasevolume_overaweek_middle$day,levels=c("Monday", 
                                                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

df_purchasevolume_overaweek_middle_ordered<-df_purchasevolume_overaweek_middle[order(df_purchasevolume_overaweek_middle$day),]

### line plot

val_middle<-c(df_purchasevolume_overaweek_middle_ordered$volume_weekly)
plot(val_middle, type="o",xlab="day of the week",ylab="transaction volume (purchase) of middle-aged adults",
     main=" Purchase transaction volume over a week of middle-aged adults")

##===========
## old
##===========
df_purchasevolume_overaweek_old<-df_sales_no_outliers %>% filter(age_group == "older adults") %>%group_by(date,day)%>%
  summarise(number_transaction_day=n())%>%group_by(day)%>%summarise(volume_weekly=mean(number_transaction_day))


### change weekday to factors and sort 
df_purchasevolume_overaweek_old$day<-factor(df_purchasevolume_overaweek_old$day,levels=c("Monday", 
                                                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

df_purchasevolume_overaweek_old_ordered<-df_purchasevolume_overaweek_old[order(df_purchasevolume_overaweek_old$day),]

### line plot

val_old<-c(df_purchasevolume_overaweek_old_ordered$volume_weekly)
plot(val_old, type="o",xlab="day of the week",ylab="transaction volume (purchase) of older adults",
     main=" Purchase transaction volume over a week of older adults")

##===============
## over all plot (for all 3 aged group)
##==============
x<-c(1,2,3,4,5,6,7)
y1<-val_young
y2<-val_middle
y3<-val_old

plot(x, y1, type="o", col="blue", pch="o",xlab="day of the week", ylab="transaction volume (purchase)",
     main="Transaction volume (purchase) over a week",lty=1,ylim=range( c(y1, y2,y3)))
points(x, y2, col="red", pch="*")
lines(x, y2, col="red",lty=2)

points(x, y3, col="dark red",pch="+")
lines(x, y3, col="dark red", lty=3)

#add a legend in top left corner of chart at (x, y) coordinates = (1, 19)
legend(1,38,legend=c("young","middle-aged","old"), col=c("blue","red","black"),
       pch=c("o","*","+"),lty=c(1,2,3), ncol=1)
######################### over a day ####################################################

##===========
##young
##===========

df_purchasevolume_overaday_young<-df_sales_no_outliers %>% filter(age_group == "young adults") %>%group_by(date,hours)%>%
  summarise(number_transaction_hour=n())%>%group_by(hours)%>%summarise(volume_daily=mean(number_transaction_hour))

### line plot

val_young_day<-c(df_purchasevolume_overaday_young$volume_daily)
plot(val_young_day, type="o",col="red",xlab="hour of the day",ylab="transaction volume (purchase) of young adults",main=" Purchase transaction volume over a day of young adults")

##==============
## middle-aged
##==============
df_purchasevolume_overaday_middle<-df_sales_no_outliers %>% filter(age_group == "middle-aged adults") %>%group_by(date,hours)%>%
  summarise(number_transaction_hour=n())%>%group_by(hours)%>%summarise(volume_daily=mean(number_transaction_hour))


val_middle_day<-c(df_purchasevolume_overaday_middle$volume_daily)
plot(val_middle_day, type="o",col="red",xlab="hour of the day",ylab="transaction volume (purchase) of middle-aged adults",
     main=" Purchase transaction volume over a day of middle-aged adults")

###============
##   old 
## ============
df_purchasevolume_overaday_old<-df_sales_no_outliers %>% filter(age_group == "older adults") %>%group_by(date,hours)%>%
  summarise(number_transaction_hour=n())%>%group_by(hours)%>%summarise(volume_daily=mean(number_transaction_hour))


val_old_day<-c(df_purchasevolume_overaday_old$volume_daily)
plot(val_old_day, type="o",col="red",xlab="hour of the day",ylab="transaction volume (purchase) of older adults",
     main=" Purchase transaction volume over a day of older adults")


### over all plot 

S


##==============================================================================
## visualise transaction volume (purchase) of gender over a week and day
##=============================================================================

############################### over a week #################################

##========
## male 
##========
df_purchasevolume_overaweek_male<-df_sales_no_outliers %>% filter(gender == "M") %>%group_by(date,day)%>%
  summarise(number_transaction_day=n())%>%group_by(day)%>%summarise(volume_weekly=mean(number_transaction_day))


### change weekday to factors and sort 
df_purchasevolume_overaweek_male$day<-factor(df_purchasevolume_overaweek_male$day,levels=c("Monday", 
                                                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

df_purchasevolume_overaweek_male_ordered<-df_purchasevolume_overaweek_male[order(df_purchasevolume_overaweek_male$day),]

### line plot

val_male<-c(df_purchasevolume_overaweek_male_ordered$volume_weekly)
plot(val_male, type="o",col="red",xlab="day of the week",ylab="transaction volume (purchase) of Male",main=" Purchase transaction volume over a week of Male")



##===========
## female
##===========


df_purchasevolume_overaweek_female<-df_sales_no_outliers %>% filter(gender == "F") %>%group_by(date,day)%>%
  summarise(number_transaction_day=n())%>%group_by(day)%>%summarise(volume_weekly=mean(number_transaction_day))


### change weekday to factors and sort 
df_purchasevolume_overaweek_female$day<-factor(df_purchasevolume_overaweek_female$day,levels=c("Monday", 
                                                                                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

df_purchasevolume_overaweek_female_ordered<-df_purchasevolume_overaweek_female[order(df_purchasevolume_overaweek_female$day),]

### line plot

val_female<-c(df_purchasevolume_overaweek_female_ordered$volume_weekly)
plot(val_female, type="o",col="red",xlab="day of the week",ylab="transaction volume (purchase) of Female",main=" Purchase transaction volume over a week of Female")


########### over all plot of both gender over a week transaction (purchase)

#x<-df_purchasevolume_overaweek_male$day
x<-c(1,2,3,4,5,6,7)
y1<-val_male
y2<-val_female



plot(x, y1, type="o", col="blue", pch="o",xlab="day of the week", ylab="transaction volume (purchase)",
     main="Transaction volume (purchase) over a week",lty=1,ylim=range( c(y1, y2)))
points(x, y2, col="red", pch="*")
lines(x, y2, col="red",lty=2)


#add a legend in top left corner of chart at (x, y) coordinates = (1, 19)
legend(1,48,legend=c("Male","Female"), col=c("blue","red"),
       pch=c("o","*"),lty=c(1,2), ncol=1)

############################# over a day ###################################

## male 
df_purchasevolume_overaday_male<-df_sales_no_outliers %>% filter(gender == "M") %>%group_by(date,hours)%>%
  summarise(number_transaction_hour=n())%>%group_by(hours)%>%summarise(volume_daily=mean(number_transaction_hour))


val_male_day<-c(df_purchasevolume_overaday_male$volume_daily)
plot(val_male_day, type="o",col="red",xlab="hour of the day",ylab="transaction volume (purchase) of Male",main=" Purchase transaction volume over a Day of Male")

## female 
df_purchasevolume_overaday_female<-df_sales_no_outliers %>% filter(gender == "F") %>%group_by(date,hours)%>%
  summarise(number_transaction_hour=n())%>%group_by(hours)%>%summarise(volume_daily=mean(number_transaction_hour))


val_female_day<-c(df_purchasevolume_overaday_female$volume_daily)
plot(val_female_day, type="o",col="red",xlab="hour of the day",ylab="transaction volume (purchase) of Female",main=" Purchase transaction volume over a Day of Female")

## overall

x<-df_purchasevolume_overaday_male$hours
y1<-val_male_day
y2<-val_female_day



plot(x, y1, type="o", col="blue", pch="o",xlab="hour of the day", ylab="transaction volume (purchase)",
     main="Transaction volume (purchase) over a day",lty=1,ylim=range( c(y1, y2)))
points(x, y2, col="red", pch="*")
lines(x, y2, col="red",lty=2)


#add a legend in top left corner of chart at (x, y) coordinates = (1, 19)
legend(1,4,legend=c("Male","Female"), col=c("blue","red"),
       pch=c("o","*"),lty=c(1,2), ncol=1)



##==========================================================================================
## Visualise transaction volume (purchase) aka POS/POS_SALES over the course of a week
##=======================================================================================


df_purchasevolume_overaweek<-df_sales_no_outliers %>%  group_by(date,day)%>%summarise(number_transaction_day=n())%>%
  group_by(day)%>%summarise(volume_weekly=mean(number_transaction_day))



df_purchasevolume_overaweek$day<-factor(df_purchasevolume_overaweek$day,levels=c("Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))


df_purchasevolume_overaweek_ordered<-df_purchasevolume_overaweek[order(df_purchasevolume_overaweek$day),]

### line plot

values_week<-c(df_purchasevolume_overaweek_ordered$volume_weekly)
plot(values_week, type="o",xlab="day of the week",ylab="transaction volume",main="Transaction volume (purchase) over a week")




##=====================================================================================================
##For a challenge - what insights can you draw from the location information provided in the dataset?
##================================================================================================

## volume by merchant state
df_merchant_state<-df_sales_no_outliers %>%  group_by(merchant_state) %>%summarise(transaction_vol=n())

name<-c("ACT", 
        "NSW", "NT", "QLD", "SA", "TAS","VIC","WA")

barplot(df_merchant_state$transaction_vol,names.arg=name,xlab="Merchant state",ylab="transaction volume",main="Transaction volume in each merchant state")


## amount by merchant state
df_merchant_state_amt<-df_sales_no_outliers %>%  group_by(merchant_state) %>%summarise(transaction_amt=sum(amount))

mrct_state_amt<-c(df_merchant_state_amt$transaction_amt)
plot(df_merchant_state_amt$merchant_state,mrct_state_amt, type="o",col="red",xlab="Merchant state",ylab="transaction amount",main="Transaction amount of each merchant state")
name<-c("ACT", 
        "NSW", "NT", "QLD", "SA", "TAS","VIC","WA")
axis(1, at=1:8, labels=name)

