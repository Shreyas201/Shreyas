rm(list=ls())

# set working directory based on where the csv file is stored in your computer
#setwd("C:/Users/shreyas/Documents/Data Science/Project - 2/test")

# Loading Libraries
x = c("ggplot2", "corrgram","DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "MASS", "rpart", "gbm", "ROSE" )
lapply(x, require, character.only = TRUE)
rm(x)

# Loading CSV File
test_data = read.csv("test.csv", header = T, na.strings = c(" ", "", "NA"))
train_data = read.csv("train_cab.csv", header = T, na.strings = c(" ","", "NA"))

str(train_data)

#Data Manipulation : converting categorical variables into factor numeric type
train_data$fare_amount = as.numeric(train_data$fare_amount)

if(class(train_data[,2]) == "factor"){
    train_data[,2] = factor(train_data[,2], labels = (1:length(levels(factor(train_data[,2])))))
  }
###Imputing Missing Value
#checking if there is any missing value
missing_val = data.frame(apply(train_data, 2, function(x){sum(is.na(x))}))
missing_val$columns = row.names(missing_val)
row.names(missing_val) = NULL
names(missing_val)[1] = "missing_percentage"
missing_val$missing_percentage = (missing_val$missing_percentage/nrow(train_data))*100
missing_val = missing_val[order(-missing_val$missing_percentage),]
missing_val = missing_val[,c("columns", "missing_percentage")]


#Creating a framework to choose best method for the imputation
#train_data[3,4] is selected for the framework
#Actual Value = 40.76127
#Mean method = 39.91467
#Medain method = 40.7526
#KNN method = 40.73314

# Trial to choose best method
mean(train_data$pickup_latitude, na.rm = TRUE)
median(train_data$pickup_latitude, na.rm = TRUE)
knnImputation(train_data, k = 5)

#knnImputation method is choosen for replaceing missing values
train_data = knnImputation(train_data, k = 5)

#Check if there are still any missing value
sum(is.na(train_data))

###Applying data pre-processing methods to remove unncessary variables/observations
#Outlier Analysis
df = train_data

numeric_index = sapply(train_data, is.numeric)
numeric_data = train_data[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "fare_amount"), data = train_data)+ 
           stat_boxplot(geom = "errorbar", width = 0.5)+ 
           geom_boxplot(outlier.colour = "red", fill = "grey", outlier.shape = 18, 
                        outlier.size = 1, notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y=cnames[i], x="fare_amount")+
           ggtitle(paste("Box Plot")))
}

#plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3, ncol=3)

# Remove Outliers
val = train_data$pickup_longitude[train_data$pickup_longitude%in%boxplot.stats(train_data$pickup_longitude)$out]
train_data = train_data[which(!train_data$pickup_longitude%in%val),]

# Loop to remove outlier from all the variables
for (i in cnames){
  print(i)
  val = train_data[,i][train_data[,i]%in%boxplot.stats(train_data[,i])$out]
  train_data = train_data[which(!train_data[,i]%in%val),]
}

###Preparing model to check for collinearity among the variables and to train the model
#checking and plotting for Correlation
corrgram(train_data[,numeric_index],order = F, 
         upper.pannel = panel.pie, text.panel = panel.txt, main = "Correlation Plot")

#Dimension reduction
train_data_deleted = subset(train_data,
                            select = -(passenger_count))

# Normalisation or Standardisation
qqnorm(train_data$pickup_longitude)
hist(train_data$dropoff_longitude)

# Standardisation
#conames = c("pickup_longitude", "pickup_latitude", "dropoff_longitude", "dropoff_latitude")
#for (i in conames){
 # print(i)
  #train_data_deleted[,i] = (train_data_deleted[,i]-mean(train_data_deleted[,i]))/
   #                         sd(train_data_deleted[,i])
#}

# Normalistion
conames = c("pickup_longitude", "pickup_latitude", "dropoff_longitude", "dropoff_latitude")

for( i in conames){
  print(i)
  train_data_deleted[,i] = (train_data_deleted[,i] - min(train_data_deleted[,i]))/
                          (max(train_data_deleted[,i] - min(train_data_deleted[,i])))
}

# Multiple Linear Regression
# Check Multicollinearity
# dividing into train and test data by 80-20 
train_index = sample(1:nrow(train_data_deleted), 0.8*nrow(train_data_deleted))
train = train_data_deleted[train_index,]
test = train_data_deleted[-train_index,]

library(usdm)

vif(train_data_deleted[,c(-1, -2)])

vifcor(train_data_deleted[,c(-1, -2)], th = 0.9)

# run regression model
lm_model = lm(fare_amount~., data = train[-2])

summary(lm_model)

predictions_LR = predict(lm_model, test[,-1])

mape(test[,1], predictions_LR)
#Error Rate = 3.95%
#Accuracy96.05

# Using Decision Tree for predicting target variable
# dividing into train and test data by 80-20 

train_index = sample(1:nrow(train_data_deleted), 0.8*nrow(train_data_deleted))
train = train_data_deleted[train_index,]
test = train_data_deleted[-train_index,]

# rpart for regression
fit = rpart(fare_amount~., data = train, method = "anova")

#predict for new test cases
predictions_DT = predict(fit, test[,-1])

# Error metrics using MAPE
mape = function(y, yhat){
            mean(abs((y-yhat)/y))
}

mape(test[,1], predictions_DT)
# Error Rate = 5.53%
# Accuracy = 94.46%
# Thus this model can be used for the prediction

# now predict for test_data. below given any one of the method can be chosen
#predictions_DT = predict(fit, test_data[,-1])
#predictions_LR = predict(lm_model, test[,1])