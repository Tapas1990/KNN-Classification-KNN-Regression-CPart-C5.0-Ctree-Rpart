head(iris)
dim(iris)#150 rows and 5 columns
summary(iris)
d1=dist(iris[1:10,c(1:4)])
d1
iris[1:2,]
((5.1-4.9)^2+(3.5-3.0)^2+0+0)^0.5#dis btwn 1 and 2 in the dist matrix
#for 1:5,8 and 10 are nearest neighbours
#in eucledian those features who hv high values hv high weightage
#so ,we need to normalize

#Normalize
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normalized_iris=lapply(iris[,-5],normalize)
iris_n=data.frame(normalized_iris,Species=iris[,5])
head(iris_n)
summary(iris_n)

d2=dist(iris_n[1:10,c(1:4)])
d2
#After normalization,neighbours change to 5,8 and 3 instead of 10

#knn
#Create the training and test data
library(caret)
set.seed(100)
train_rows=createDataPartition(iris_n$Species,p=0.7,list=F)
train=iris_n[train_rows,]
test=iris_n[-train_rows,]

library(class)#for the loss function
out=knn(train[,-5],test[,-5],train[,5],k=5)
out

fit=caret::knn3(train[,-5],train[,5],k=5)
#summarize the fit
print(fit)
#make predictions
predictions=predict(fit,test[,-5],type='class')
tab=table(predictions,test$Species)
caret::confusionMatrix(tab)

mean(test[,5]!=out)
sum(test[,5]!=out)

#knn regression
#knn for regression with caret
data(Boston,package='MASS')
head(Boston)
set.seed(100)
#prepare training and test data
train_rows=caret::createDataPartition(Boston$medv,p=0.7,list=F)
train=Boston[train_rows,]
test=Boston[-train_rows,]

fit=knnreg(train[,-14],train[,14],k=5)#x,y format
#summarise the fit
print(fit)
#make predictions
predictions=predict(fit,test[,-14])
head(predictions)
DMwR::regr.eval(test$medv,predictions)

#for wat value of k(<20),does k-NN achieve the maximum kappa for iris data set?
train_rows=createDataPartition(iris_n$Species,p=0.7,list=F)
train=iris_n[train_rows,]
test=iris_n[-train_rows,]
k_vals=c(5,7,11,13,17,19)#cant use k as even no or multiples of 3
kappa_out=numeric(6)

for(i in 1:length(k_vals)){
  out=knn(train[,-5],test[,-5],train[,5],k=k_vals[i])
#evaluation
  tab=table(out,test$Species)
  mat=caret::confusionMatrix(tab)
  kappa_out=mat$overall[2]
} 
k_vals[which.max(kappa_out)]#value==5

#Tree based models using Rpart,Ctree and C5.0
data(iris)
train_rows=createDataPartition(iris$Species,p=0.7,list=F)
train=iris[train_rows,]
test=iris[-train_rows,]

library(partykit)
ctMod=ctree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=train)
#3 inner nodes ,4 terminal nodes
print(ctMod)
plot(ctMod)
ctMod_2=ctree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,control=ctree_control(maxdepth=2),data=train)
#2 inner nodes:3 terminal nodes
print(ctMod_2)
plot(ctMod_2)
out=predict(ctMod_2,test)
#compare
mean(test[,5]!=out)
sum(test[,5]!=out)

#Rpart
#control parameters-cp,minsplit,maxdepth,xval
library(rpart)
rpartMod=rpart(Species~.,data=train,control = rpart.control(minsplit = 5,cp=0,maxdepth=4))
rpartMod

pred=predict(rpartMod,test,type='class')
pred
mean(pred!=as.character(test$Species))

library(rattle)
library(RColorBrewer)
fancyRpartPlot(rpartMod)#rattle lib

#Ctree Plot
library(partykit)
iris_party=as.party.rpart(rpartMod)
plot(iris_party)

#rpart.plot
library(rpart.plot)
prp(rpartMod,extra = 1,type = 2)

#prune
rpartmod_pruned=prp(rpartMod,snip = TRUE)$obj#interactively trim the tree
prp(rpartmod_pruned,extra=1,type=2)#click on quit ,prune tree is stored in prpobject.

#C5.0
library(C50)
C5Mod=C5.0(Species~.,data=train,control=C5.0Control(winnow=F))
summary(C5Mod)
plot(C5Mod)
C5imp(C5Mod)#importance of vars

#Rules mod
C5Mod_rules=C5.0(Species~.,data=train,rules=T,
                 control=C5.0Control(winnow = F))
summary(C5Mod_rules)

#Predict if a patient has glaucoma or not using
#the ctree algorithm,from 'glaucomaM'data set in TH.data package
data('GlaucomaM',package = 'TH.data')
head(GlaucomaM)


#Split training and test data
library(caret)
set.seed(100)
train_rows=caret::createDataPartition(GlaucomaM$Class,p=0.7,list=F)
traindata=GlaucomaM[train_rows,]
testdata=GlaucomaM[-train_rows,]

#ctree mod
library(partykit)
cMod=ctree(Class~.,data=traindata)
print(cMod)
plot(cMod)
pred_ctree=predict(cMod,testdata)
mean(pred_ctree!=testdata$Class)

#C5Mod
library(C50)
C5Mod=c=C5.0(Class~.,data = traindata)
summary(C5Mod)
plot(C5Mod,gp=gpar(fontsize=6))#reduce the font size
pred_c5=predict(C5Mod,testdata)
mean(pred_c5!=testdata$Class)
