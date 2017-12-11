install.packages("e1071")
library(e1071)
?tune # tune `svm' for classification with RBF-kernel (default in svm)

###example of svm application
head(iris,5)
attach(iris)

x <- subset(iris, select=-Species)
y <- Species

svm_model <- svm(Species ~ ., data=iris)
summary(svm_model)
#Or
svm_model1 <- svm(x,y)
summary(svm_model1)
#

pred <- predict(svm_model1,x)
system.time(pred <- predict(svm_model1,x))

table(pred,y)

svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

svm_model_after_tune <- svm(Species ~ ., data=iris, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)

pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))

table(pred,y)

###
setwd("movie_metadata.csv")    #!why there is always wrong with reading files
getwd()

movie_data<-read.csv("/Users/fanliu/Desktop/movie_metadata.csv") #finally, maybe you need to getwd first and then read.csv
head(movie_data)
names(movie_data)
movies_with_good_variables = movie_data[, c("imdb_score",
                                        "director_facebook_likes", 
                                        "cast_total_facebook_likes", 
                                        "actor_1_facebook_likes",
                                        "actor_2_facebook_likes",
                                        "actor_3_facebook_likes",
                                        "movie_facebook_likes", 
                                        "facenumber_in_poster",
                                        "gross",
                                        "budget")]
mvs = na.omit(movies_with_good_variables)
x = as.matrix(mvs[, -1])
y = mvs[, 1]
summary(y)
boxplot(y)
length(y)
y[1]

#Using cut to categorize numeric values
y<- cut(y, breaks = c(1.5, 4, 6.6, 9.4), labels = c("1", "2", "3"), right = FALSE)
class(y)
head(y)
y<-as.character(y) 
y<-as.factor(y)
head(y)
#training and testing data

n<-length(y)
ntrain <- round(n*0.60) # number of training examples
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]


svm_model1<-svm(xtrain,ytrain,type = "C-classification")  
mvs$imdb_score<-y
View(mvs)
data<-cbind(xtrain,ytrain)
View(data)

plot(svm_model1,data) #can't plot....

pred <- predict(svm_model1,xtrain)
table(pred,ytrain)


svm_tune <- tune(svm, ytrain~xtrain, ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
summary(svm_tune)
#best parameters: C=1, gamma=0.5

#train the model
model<- svm(xtrain,ytrain, kernel="radial", gamma = 0.5, cost = 1) 
summary(model)


#test the model
prediction <- predict(model, xtest)
prediction
head(prediction)
class(prediction)
class(ytest)

test<-ytest==prediction
length(test[test=="TRUE"])
length(test)
length(test[test=="TRUE"])/length(test)
#10/90
#the accuracy is 0.63


#20/80
#the accuracy is 0.65

#30/70
#the accuracy is 0.635

#40/60
#the accuracy is 0.623


install.packages("caret")
library(caret)
sensitivity(Tabs,"1")
sensitivity(Tabs,c("2","3"))
sensitivity(Tabs,"3")

specificity(Tabs,"1")
specificity(Tabs,c("2","3"))
specificity(Tabs,"3")
Tabs
