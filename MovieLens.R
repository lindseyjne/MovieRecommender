#Building a recommendation system

setwd("d:/Movie Data/ml-100k")

#read the data and view the first six observations
data <- read.table('u.data')
head(data)

#rename the columns to reflect the data in the column
colnames(data) = c("user_id", "item_id", "rating", "timestamp")

#since I am not going to use timestamps, I will drop the variable
data=data[, -which(names(data) %in% c("timestamp"))]

#look at the data properties
str(data)
summary(data)

#plot a histogram of the ratings to see the distribution
hist(data$rating)

#calculating the data sparsity
Number_Ratings <- nrow(data) #number of ratings 
Number_Movies <- length(unique(data$item_id)) #number of rated movies
Number_Users <- length(unique(data$user_id)) #number of users rating movies
Sparsity <- ((Number_Ratings) / (Number_Movies * Number_Users)) * 100

#Subseting the data to be less sparse
#Subseting the data to include only users with 50 or more ratings
data <- data[data$user_id %in% names(table(data$user_id))[table(data$user_id) > 50],]

#Splitting the data randomly to create a train and test dataset 
#70/30 split for train/test
library(caTools)
library(recommenderlab)
library(ggplot2)

#split the data to train/test at 70%/20%
spl <- sample.split(data$rating, 0.7)
train2 <- subset(data, spl == TRUE)
test <- subset(data, spl == FALSE)

#split the train data to train/validate at 75%/20%
spl2 <- sample.split(train2, 0.75)
train <- subset(train2, spl2 == TRUE)
validate <- subset(train2, spl2 == FALSE)


dataMatrix <- as(train, "matrix")
dataMatrix <- as(train, "realRatingMatrix")

#view the first few lines of the matrix as a dataframe
head(as(dataMatrix, "data.frame"))

#normalize the matrix e.g. centering to remove rating bias by subtracting the row mean from all ratings in the row
dataMatrix.normalize <- normalize(dataMatrix)

image(dataMatrix, main = "Raw Ratings")
image(dataMatrix.normalize, main = "Normalized Ratings")

Rec.model <- Recommender(dataMatrix[1:563], method = "UBCF")

Rec.model <- Recommender(dataMatrix[1:563], method = "UBCF", param=list(normalize = "Z-score", method="Cosine", nn=5, minRating=1))

