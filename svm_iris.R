#rload dataset
data<-iris
head(data)
tail(data)

# summarize the class distribution
percentage <- proportions(table(data$Species)) * 100 # Alternatif : prop.table
cbind(freq=table(data$Species), percentage=percentage)

#check levels of Species
levels(data$Species)

#check missing value
sum(is.na(data))

# summarize attribute distributions
summary(data)

# Create Data Partition
library(caret)
set.seed(123)
index <- createDataPartition(data$Species, p=0.80, list=FALSE)

# select 80% of the data for Training
training <- data[index,] # Data Frame [baris, kolom]
dim(training)

# use the remaining 20% of data to testing the models
testing <- data[-index,]
dim(testing)

##### SVM
svm_model <- svm(Species ~ ., training,
                 kernel="radial") #linear/polynomial/sigmoid
plot(svm_model, training,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)
pred = predict(svm_model,training)
# Confusion Matrix
cm <- table(training$Species, pred)
cm
# Model Evaluation
confusionMatrix(cm)

pred = predict(svm_model,testing)

# Confusion Matrix
cm_svm <- table(testing$Species, pred)
cm_svm
# Model Evaluation
confusionMatrix(cm_svm)