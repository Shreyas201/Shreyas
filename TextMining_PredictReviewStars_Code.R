rm(list=ls())

setwd("C:/Users/shreyas/Documents/Data Science/Text Mining Project")

# Loading Libraries
x = c("stringr", "tm", "wordcloud", "slam")
lapply(x, require, character.only = TRUE)
rm(x)

#Loading of CSV File
yelp = read.csv("yelp.csv", header =T)

#Creating a data frame with only two variable i.e text and stars extracted from yelp dataset
df = data.frame(yelp$text, yelp$stars)
colnames(df) = c("text", "stars")
df$stars[df$stars >= 0 & df$stars < 3] = "Negative"
df$stars[df$stars >= 3 & df$stars <= 5] = "Positive"
df$stars = as.factor(df$stars)

#Pre-Processing Methods
#Delete leading spaces
df$text = str_trim(df$text)

#Convert text into corpus
dfCorpus = Corpus(VectorSource(df$text))

#Case folding
dfCorpus = tm_map(dfCorpus, tolower)

#Remove stop words
dfCorpus = tm_map(dfCorpus, removeWords, stopwords("english"))

#Remove punctuation
dfCorpus = tm_map(dfCorpus, removePunctuation)

#Remove numbers
dfCorpus = tm_map(dfCorpus, removeNumbers )

#Remove unnecesary spaces
dfCorpus = tm_map(dfCorpus, stripWhitespace)

#Converting unstructured data to structured data
tdm = TermDocumentMatrix(dfCorpus)

sparse = removeSparseTerms(tdm, 0.99)

#Convert term document matrix into dataframe
TDM_Data = as.data.frame(t(as.matrix(sparse)))


#Creating a data frame that is going to be used for model building
df_stars = data.frame(df$stars)
colnames(df_stars) = "sentiment"
final_data = cbind(TDM_Data, df_stars)

#Preparing model using naiveBayes method
library(e1071)
#Dividing data into train and test 
train_index = sample(1:nrow(final_data), 0.8*nrow(final_data))
train = final_data[train_index,]
test = final_data[-train_index,]

#Model Development
NB_model = naiveBayes(sentiment~., data = train)

#Predict on test cases
NB_predictions = predict(NB_model, test[,1:995], type = 'class')

#Constructing Confusion Matrix
library(lattice)
#install.packages("rlang")
library(rlang)
library(ggplot2)
library(caret)
Conf_matrix = table(test[,996],NB_predictions)
confusionMatrix(Conf_matrix)

#Accuracy = 81.85%
#FNR = 40.79%

# Get the output of tabular data of the final data that is going to be used for model building
#install.packages("xlsx")
library(xlsx)
#Final_data1 = write.xlsx(final_data, "TM-Shreyas_Ainapur.xlsx", sheetName = "final_data")
Final_data1 = write.csv(final_data, "TM-Shreyas_Ainapur.csv")