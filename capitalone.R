
#==================================================================
# Install packages not already installed in a list
#==================================================================


list=c("tidyverse","stringr","forcats","ggmap","rvest","tm","SnowballC","dplyr","calibrate","doParallel",
       "stringi","ggplot2","maps","httr","rsdmx","devtools","plyr","dplyr","ggplot2","caret","elasticnet",
       "magrittr","broom","glmnet","Hmisc",'knitr',"RSQLite","RANN","lubridate","ggvis","plotly","lars",
       "ggcorrplot","GGally","ROCR","lattice","car","corrgram","ggcorrplot","sqldf","parallel")



list_packages <- list
new.packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(list, require, character.only = TRUE)

#==================================================================
#  Set up parallel processing
# leave two cores for operating system
#==================================================================

cluster <- makeCluster(detectCores() - 2) 
registerDoParallel(cluster)


#==================================================================
#display all coumns of data with dplyr
# Print first 1000 rows of dataset
#==================================================================

options(dplyr.width = Inf)

options(dplyr.print_max = 1000)


#==================================================================
# Look at  the  structure of the data with the glimpse function in 
#  dplyr  package
#==================================================================

dplyr::glimpse(data)






#==================================================================
# load the downloaded data with the readr package
#  dim function produces the dimension of the data
# the  September 2015 data contains 1048575  rows  and      21 columns
#==================================================================

data=readr::read_csv("C:/Users/Gucci148/Documents/DataMiningscience/Capitalone/green_tripdata_2015-09.csv")

names(data)

dim(data)


summary(data)



#knitr::kable(do.call(rbind,apply(data,2,Hmisc::describe)))







#==================================================================
# 
#  1 check missing obervations of each column variable
#  2 sum of all missing obervations in data
#  3   Ehail_fee  has all rows missing,remove from dataset
#==================================================================



apply(is.na.data.frame(data),2,sum)

sum(is.na(data))

newdata=dplyr::select(data,-Ehail_fee)

sum(is.na(newdata))




#==================================================================
# 
# Histogram of the Distribution of Trip Distance
# with ggplot2 package
#Make the plot interactive with ggplotly
#==================================================================

p<-ggplot(newdata, aes(x =Trip_distance )) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.02) + 
theme_minimal() + ggtitle(" Histogram of the Distribution of Trip Distance") +
  #center title
  theme(plot.title = element_text(hjust = 0.5)) +xlab("Trip Distance")+ylab("Frequency")


ggplotly(p)



# The average elapsed trip distance  reported by the taximeter is approximately 3 miles
# The median elapsed trip distance  reported by the taximeter is approximately 2 miles 
# # The highest  elapsed trip distance  reported by the taximeter is approximately 603 miles and lowest is 0. 
# the 603 mile appears to be an outlier




#==================================================================
# 
# Histogram of the Distribution of Log Trip Distance
# with ggplot2 package
#==================================================================

newdata=newdata%>%mutate(New_distance=log(Trip_distance+1)) 

p=ggplot(newdata, aes(x =New_distance ))+  geom_histogram(fill="black",col="black",binwidth=0.5) +
  theme_minimal() + ggtitle(" Histogram of the Distribution of Trip Distance") +
  #center title
  theme(plot.title = element_text(hjust = 0.5)) +xlab("Log of Trip Distance")+ylab("Frequency")

ggplotly(p)


#==================================================================
# 
# Normal qqplot
#==================================================================


ggplot(newdata, aes(sample = New_distance))+ stat_qq()



#===========================================================================
# 
# Mean and median trip distance grouped by hour of day.
# Create Hours variable by extracting hours from the 
#  the  date and time when the meter was engaged (lpep_pickup_datetime).
#===========================================================================

newdata=newdata%>%dplyr::mutate(Hours= format(as.POSIXct(strptime(newdata$lpep_pickup_datetime,"%m/%d/%Y %H:%M",tz="")) ,format = "%H"))

newdata%>% mutate(Hours=as.numeric(Hours))%>%group_by(Hours)%>% dplyr::summarise(Mean=mean(Trip_distance),Median=median(Trip_distance),n=n())
  

#===========================================================================
# Find the Hour in which  the  maximum mean  Trip distance occured
#===========================================================================

newdata%>% mutate(Hours=as.numeric(Hours))%>%group_by(Hours)%>% dplyr::summarise(Mean=mean(Trip_distance),Median=median(Trip_distance),n=n())%>%
  dplyr::slice(which.max(Mean )) 



#===========================================================================
# Find the Hour in which  the  maximum  median  Trip distance occured
#===========================================================================

newdata%>% mutate(Hours=as.numeric(Hours))%>%group_by(Hours)%>% dplyr::summarise(Mean=mean(Trip_distance),Median=median(Trip_distance),n=n())%>%
  dplyr::slice(which.max(Median )) 


#==================================================================
# 
# derived variable for tip as a percentage of the total fare
#==================================================================


newdata=newdata%>%dplyr::mutate(Tip=(Tip_amount/Total_amount)*100)


# Alternatively

#newdata=newdata%>%dplyr::transmute(Tip=(Tip_amount/Total_amount)*100)




#==================================================================
# 
# Building a Predictive Model
# 1 select variables for the model
# 
#==================================================================






#==============================================================================
# Preprocessing data
# 1 There are three missing variables that will be removed at preprocessing step
# 2  Center and scale numerical columns to reduce variability
#==============================================================================


newdata=newdata[complete.cases(newdata),]

sum(is.na(newdata))


summary(newdata)








#==================================================================
#Spliting training set into two parts based on outcome:
# Training set which 70% and Test set which 30% of the data
#==================================================================


newdata$VendorID=as.numeric(newdata$VendorID)

index <- createDataPartition(newdata$Tip_amount, p=0.70, list=FALSE)

trainSet <- newdata[ index,]

testSet <- newdata[-index,]


sum(is.na(trainSet))
sum(is.na(testSet))
sum(is.na(newdata))


#==================================================================
#  Exploratory data analysis
#==================================================================




newdata%>%dplyr::select(Trip_distance,Fare_amount,improvement_surcharge,Tolls_amount,Tip_amount,MTA_tax
                        ,Extra,Passenger_count,VendorID, Tip,Total_amount)%>%cor

# names(newdata)
#There are very  high positive  correlations between Total amount and Trip distance,of course which makes sense 
# Total amount and Fare amount that passengers who travelled longer distances  were charged more.
# There is also a significant positive correlation between Total amount and Tip amount.
# Passengers who travelled longer distances and paid higher fares were more likely to tip.

ggplot(trainSet , aes(Tip, Tip_amount))+geom_point(color="purple")+
  ggtitle('Tip vs Tip_amount')+
  xlab('Tip')+ylab('Tip_amount')+
  stat_smooth(method=lm, colour='black',span=0.2)





# There exist a strong association between Tip and Tip amount

ggplot(trainSet , aes(Tip, Fare_amount))+geom_point(color="purple")+
  ggtitle('Tip vs Fare_amount')+
  xlab('Tip')+ylab('Fare_amount')+
  stat_smooth(method=lm, colour='black',span=0.2)


ggplot(trainSet , aes(Tip, Trip_distance))+geom_point(color="purple")+
  ggtitle('Tip vs Trip_distance')+
  xlab('Tip')+ylab('Trip_distance')+
  stat_smooth(method=lm, colour='black',span=0.2)

# The figure shows there are a few outlier points ,where the Tip was low for a very long Trip distance

ggplot(trainSet , aes(Tip, Tolls_amount))+geom_point(color="purple")+
  ggtitle('Tip vs Tolls_amount')+
  xlab('Tip')+ylab('Tolls_amount')+
  stat_smooth(method=lm, colour='black',span=0.2)



# The Figure does not reveal a strong association  between Tolls amount and Tip given




#==================================================================
#  Do feature selection using a linear model 
#==================================================================


control <- trainControl(method="repeatedcv", number=10, repeats=5)


outcomevariable<-'Tip'

#predictors<-names(trainSet)[!names(trainSet) %in% outcomevariable]

#trainSet[,predictors]

my_glm<- train(trainSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax","improvement_surcharge",
                          "Extra","Passenger_count","VendorID")], trainSet$Tip,
                    method = "glm",
                    preProc = c("center", "scale"),
                    trControl = control)




summary(my_glm)

# Predict using the test data

pred<-predict(my_glm,testSet )



glm_data=data_frame(predicted=pred,observed=testSet$Tip)



ggplot(glm_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('GLM')


# Print, plot variable importance

print(varImp(my_glm, scale = FALSE))

class(print(varImp(my_glm, scale = FALSE)))

plot(varImp(my_glm, scale = FALSE), main="Variable Importance using GLM")

ggplot(varImp(my_glm, scale = FALSE))+ggtitle("Variable Importance using GLM")+theme_minimal()






#===============================================================================
#  Build a  Generalized Linear Model with Stepwise Feature Selection
#===============================================================================


control <- trainControl(method="repeatedcv", number=10, repeats=5)


step_glm<- train(trainSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                             "Extra","Passenger_count","VendorID")], trainSet$Tip,
                 method = "glmStepAIC",
                 preProc = c("center", "scale"),
                 trControl = control)


# Predict using the test data

summary(step_glm)


pred<-predict(step_glm,testSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                          "Extra","VendorID")])


glm_data=data_frame(predicted=pred,observed=testSet$Tip)



ggplot(glm_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('GLM')


# Print, plot variable importance

print(varImp(my_glm, scale = FALSE))



plot(varImp(my_glm, scale = FALSE), main="Variable Importance using GLM")



# Mean Squared Error

sqrt(mean(pred- testSet$Tip)^2)

#===============================================================================
#  Build a  Partial Least Squares Model 
#===============================================================================

# Partial Least Squares is one way to reduce dimension of the predictors used in the model. It identifies
#linear combinations,or directions, that best represent the predictors in the data.The directions are identified in 
# unsupervised way since the outcome variable is not in identifying the principal
# component directions.The predictors are preprocessed by centering and scaling.
# PLS will seek
#directions of maximum variation while simultaneously considering correlation with the response.




plsfit=train(trainSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                  "Extra","VendorID","Passenger_count")], trainSet$Tip,
      method = "glmStepAIC",tuneLength = 20,
      preProc = c("center", "scale"),
      trControl = control)






# Predict using test data


pred<-predict(plsfit,testSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                            "Extra","VendorID")])

my_data=data_frame(predicted=pred,observed=testSet$Tip)

ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('PLS')

# Print, plot variable importance
print(varImp(plsfit, scale = FALSE))

plot(varImp(plsfit, scale = FALSE), main="Variable Importance using PLS")

summary(plsfit)

plot(plsfit)


# Mean Squared Error

sqrt(mean(pred- testSet$Tip)^2)

#===============================================================================
#  Build Penalized linear regression models Model 
# Lasso (least absolute shrinkage and selection operator)
#===============================================================================

# The LASSO  penalizes the model  for having many predictors by shrinking the coefficients of 
# 
# those predictors with little variation to zero thereby reducing the dimension of the model.

lassoGrid <- expand.grid(.fraction = seq(.05, 1, length = 20))

control <- trainControl(method="repeatedcv", number=10, repeats=5)

set.seed(100)






lassofit<-train(trainSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                  "Extra","VendorID","Passenger_count")], trainSet$Tip,
      method = "lasso",tuneGrid = lassoGrid,
      preProc = c("center", "scale"),
      trControl = control)


# Predict using test data
pred<-predict(lassofit,testSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                                  "Extra","VendorID")])

my_data=data_frame(predicted=pred,observed=testSet$Tip)

ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('LASSO Model')

# Print, plot variable importance
print(varImp(lassofit, scale = FALSE))

plot(varImp(lassofit, scale = FALSE), main="Variable Importance using lasso")

plot(lassofit)

summary(lassofit)

#Extract coefficients of final model

predict.enet(lassofit$finalModel, type='coefficients', s=lassofit$bestTune$fraction, mode='fraction')


# Mean Squared Error

sqrt(mean(pred- testSet$Tip)^2)




lasso <- train(Salary ~., train,
               method='lasso',
               preProc=c('scale','center'),
               
               trControl=fitControl)

lasso<-train(trainSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                          "Extra","VendorID")], trainSet$Tip,
              method = "lasso",
              preProc = c("center", "scale"),
              trControl = control)


#===============================================================================
#  Random Forest linear regression models Model 
# 
#===============================================================================



control <- trainControl(method="repeatedcv", number=10, repeats=5)



rf_fit<-train(trainSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                  "Extra","VendorID")], trainSet$Tip,
      method = "rf", ntrees = 1000,importance = TRUE,
      preProc = c("center", "scale"),
      trControl = control)

# Predict using the test data

pred<-predict(rf_fit,testSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                                  "Extra","VendorID")])

my_data=adata_frame(predicted=pred,observed=testSet$Tip)

ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('Random Forest Model')

# Print, Plot variable importance
print(varImp(rf_fit, scale = FALSE))

plot(varImp(rf_fit, scale = FALSE),main="Variable Importance using RF")


# Mean Squared Error

sqrt(mean(pred- testSet$Tip)^2)

#===============================================================================
# Gradient Boosting Machine  Model 
# 
#===============================================================================

# To tune over interaction depth, number of trees, and shrinkage first define a tuning grid,
#then train over this grid


gbmGrid <- expand.grid(.interaction.depth = c(2,5,8),
                       .n.trees = c(500, 1000,2000,5000),
                       .shrinkage = c(0.01, 0.1),
                       .n.minobsinnode=c(5,10,15))

control <- trainControl(method="repeatedcv", number=10, repeats=5)


gbm_fit<-train(trainSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                    "Extra","VendorID")], trainSet$Tip,
        method = "gbm",verbose=F, tuneGrid = gbmGrid,
        preProc = c("center", "scale"),
        trControl = control)

# Predict using the test data

pred<-predict(gbm_fit,testSet[,c("Fare_amount","Trip_distance","Tolls_amount","MTA_tax",
                                "Extra","VendorID")])

my_data=adata_frame(predicted=pred,observed=testSet$Tip)


ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('Boosting')
# Print, Plot variable importance
print(varImp(gbm_fit, scale = FALSE))

plot(varImp(gbm_fit, scale = FALSE),main="Variable Importance using Boosting")




# collect resamples
results <- resamples(list(GLM=my_glm,StepGLM=step_glm,PLS=plsfit,
                          Lasso=lassofit,
                          Gradientboosting=gbm_fit, rf=rf_fit))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)



# Mean Squared Error

sqrt(mean(pred- testSet$Tip)^2)


#===============================================================================
#Question 5
# Option A: Distributions
#===============================================================================



#Build a derived variable representing the average speed over the course of a trip.
#Average Speed=(distance)/time

# 1 create a TimeDifference variable by subtracting  lpep_pickup_datetime
# from Lpep_dropoff_datetime using the difftime function.
#2 Extract the  numeric values  in difference  in time(in seconds) by 
# by the stri_extract_all_regex function
# 3 unlist the result which is characer and convert to numeric
# 4  convert time  in seconds  to hours by multiplying by 0.000277778
# This standardizes speed in scientific units which is miles per hour
# 5 Time difference greater than zero seconds were selected in computing the 
#  speed to avoid undefined mathematical expressions
# 6 Speed variable=Trip distance/ TimeDifference

# Average speed=mean of the Speed variable

# The Average speed over all of september is approximately 5.844  meters per second


newdata=newdata%>%mutate(TimeDifference=
              as.numeric(unlist( stri_extract_all_regex(difftime(as.POSIXct(Lpep_dropoff_datetime, format="%m/%d/%Y %H:%M", tz="")
                                                                 , as.POSIXct(lpep_pickup_datetime, format="%m/%d/%Y %H:%M", tz="")
                                                                 , tz="",units = , "secs"), "[0-9]+"))))%>%mutate(Time=TimeDifference*0.000277778)









newdata=newdata[newdata$Time>0,]

#newdata=newdata%>%mutate(Speed=Trip_distance/TimeDifference)%>%dplyr::filter[newdata,TimeDifference >0]

# The filter approach seems not to recognise TimeDifference variable just created.



newdata=newdata%>%mutate(Speed=Trip_distance/Time)

newdata%>%dplyr::summarise(Average_Speed=mean(Speed))

summary(newdata)



#======================================================================================
#  Test of the mean and Median of average trip speeds
# in all weeks of September
# 1 Create a derived variable Week for the number of weeks in Septemeber
#  present in the data
# 2  We then group speed by week 
# 3 Test for difference in Mean speed for the weeks in September.
# 4 We will perform Kruskal Wallis nonparametric test and compare with parametric ANOVA 
#test of  the mean Speeds in the 3 weeks in September. The ANOVA test assumes the data is
# normally distributed wheras Kruskal-Wallis test does not.
#======================================================================================

newdata=newdata%>%
  dplyr::mutate(dates= format(as.POSIXct(strptime(newdata$lpep_pickup_datetime,"%m/%d/%Y %H:%M",tz="")) ,format = "%m/%d/%Y"))

newdata=newdata %>%  
  mutate(Week = case_when(.$dates %in% week1 ~ 1, .$dates %in% week2 ~ 2,.$dates %in% week3 ~ 3)) 


#Alternatively
#newdata=newdata %>%  
#  mutate(week=if_else(dates %in% week1,1,if_else(dates %in% week2,2,3)))

newdata %>%group_by(Week)%>% dplyr::summarise(mean=mean(Speed))
 


newdata %>%dplyr::select( Week,Speed)%>%lm(formula = Speed ~ factor(Week ))%>%anova()%>%tidy()

newdata %>%dplyr::select( Week,Speed)%>%mutate(Week=factor(Week))%>%kruskal.test()

kruskal.test(newdata$Speed,factor(newdata$Week))

# The average trip speeds are materially the different  in all weeks of September
# There is significant difference in the average Speed  by week travelled in September(p-value  < 2.2e-16)
# Labor weekend occurred in the  first week of Septtember 2015,that could have contributed to ithe
# Higher speed in the first week of September



#========================================================================================
#   hypothesis of average trip speed as a function of time of day
# A plausible Hypothesis would be to test if speed is associated with time of the day
#  Tidy the regression output with tidyr
#========================================================================================



# A basic box with the conditions colored
newdata %>%dplyr::select(  Hours,Speed)%>%mutate( Hours=as.numeric( Hours))%>%dplyr::group_by(Hours)%>%
  ggplot( aes(x=Hours, y=Speed, fill=Hours)) + geom_point()



newdata %>%dplyr::select(  Hours,Speed)%>%mutate( Hours=as.numeric( Hours))%>%
  lm(formula = Speed ~ (Hours ))%>%summary()

fit_glm=glm( Speed ~ as.numeric(Hours), data=newdata)


tidy(fit_glm)

head(augment(fit_glm))


glance(fit_glm)
# There exist a significant association between Speed and Hour of the day
# The coefficient of Hours is negative which indicates a negative relationship
# Between the two variables. For every unit increase in Hour of the day,the 
# Speed decreases by a factor of about 0.048