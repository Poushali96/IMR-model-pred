# IMR-model-pred
Here we have create three models based on IMR (infant mortality rate) dataset and compared. themm with each other.
library(caret)
library(olsrr)
library(broom)
library(nortest)
library(MASS)
library(ggfortify)
library(car)
library(lmtest)
library(psych)
library(ggcorrplot)
setwd("C:/Users/Poushali Sengupta/Desktop/research project/GD/")
data<-read.csv("data.csv",header = T)
data
head(data)
summary(data)
data1<- data[,-1]
data1
View(data1)

##________Removing Outliers______##
boxplot(data1)
for(i in 1:21){
  outliers<-boxplot(data1[i], plot = FALSE)$out
  if(length(outliers)>0){
    x<-data1
    x<- x[-which(data1[,i] %in% outliers),]
    n=length(data1$Newborn)
    for(j in 1:n){
      if(data1[i][j,]>=outliers[1]){
        data1[i][j,]=max(x[i])
      }
    }
    
  }
}
boxplot(data1)
fullmodel<-lm(IMR~., data=data1)
fullmodel.diag.metrics1 <- augment(fullmodel)
head(fullmodel.diag.metrics1)
write.csv(head(fullmodel.diag.metrics1), file = "Desktop.csv")
read.csv("desktop.csv")

##_______ Anderson-Darling test for Normality of residuals_______##

ad.test(fullmodel.diag.metrics1$.resid)


##_____Autocorrelation______##

dwtest(fullmodel)


##____Multicolinearity____##

vif(fullmodel)
y <- cor(data1)
y
ggcorrplot(y,hc.order = F,type ="lower",lab=T,lab_size =3.2,
           lab_col = "black",outline.color = "black",
           p.mat=cor_pmat(data1), sig.level = 0.01,insig="blank" ,
           show.legend = T,ggtheme = theme_grey())
t=cor_pmat(data1)
#par(mfrow=c(1,2))
ggcorrplot(t,hc.order = F,type ="lower",lab=T,lab_size =3.2,
           lab_col = "black",outline.color = "black",
           show.legend = F,ggtheme = theme_grey())
ggcorrplot(y,hc.order = F,type ="lower",lab=T,lab_size =3.2,
           lab_col = "black",outline.color = "black",
           show.legend = T,ggtheme = theme_grey())
data2<-data1[,-c(2,3,5,9,12,13,16,17,20)]
data2
write.csv(data2, file = "data2.csv")
y1=cor(data2)
ggcorrplot(y1,hc.order = F,type ="lower",lab=T,lab_size =3.2,
           lab_col = "black",outline.color = "black",
           show.legend = T,ggtheme = theme_grey())
t1=cor_pmat(data2)
ggcorrplot(t1,hc.order = F,type ="lower",lab=T,lab_size =3.2,
           lab_col = "black",outline.color = "black",
           show.legend = F,ggtheme = theme_grey())
fullmodel2<-lm(IMR~., data = data2)
vif(fullmodel2)

##______Principle Component Analysis_____##

data2.pca<-prcomp(data2,center = TRUE, scale. = TRUE)
data2.pca
summary(data2.pca)


##____data pertition____##

set.seed(1234)
ran<-sample(1:nrow(data2),0.8*nrow(data2))
ran
training <- data2[ran,]
test <- data2[-ran,]
View(training)

##____model validation____##


trcontrol<-trainControl(method = 'repeatedcv', number = 10, repeats =1000,search = "random")
set.seed(185)
fit <- train(IMR ~ ., data = training, tuneLength= 15 ,method = 'lmStepAIC',metric= "Accuraccy", trControl=trcontrol)
fit
varImp(fit)
predict1<- predict(fit$finalModel)
predict1
predict2<- predict(fit$finalModel,newdata = test)
predict2 
predict<-c(43.42980, 23.07897,33.69209,51.55228, 20.14449,31.65128,24.95008,36.64494,48.32870,40.35760,29.79524,39.70782,28.06299,11.69473,26.17901,48.78747,33.56386,45.31384,33.12705,39.14585,43.42431,30.39141,14.74862,36.14109 ,41.34096,29.16357,41.29297,37.18901,39.05272)
predict
plot(predict~data2$IMR, xlab="Actual IMR", ylab ="prediction", main="Graph for predition values of IMR")
abline(lm(predict~data2$IMR),data=data2, col="RED")
olsrr::ols_mallows_cp(fit$finalModel, fullmodel2)
par(mfrow=c(2,2))
plot(fit$finalModel,5)
summary(fit$finalModel)
fit$finalModel
fit$results
summary(fit$finalModel)
fit$modelInfo
model<-fit$finalModel
model

##_____Linearity of the data_____##
plot(model, 1)

##_____Homogeneity of variance____##
plot(model, 3)

##____Normality of residuals____##
plot(model, 2)

##_____Outliers and high leverage points___##
plot(model, 5)

mycd<-cooks.distance(model)
mycd1<-round(mycd, 4)
sort(mycd1)
par(mfrow= c(1,2))
plot(model, 4)
plot(model, 5)

##_____Knn Model______##

set.seed(8557)
ranknn<-sample(1:nrow(data2),0.8*nrow(data2))
ranknn
trainingknn <- data2[ranknn,]
testknn <- data2[-ranknn,]
View(trainingknn)
#trcontrol<-trainControl(method = 'oob')
trcontrolknn<-trainControl(method = 'repeatedcv',number = 10,repeats = 1000)
set.seed(533)
fitknn <- train(IMR ~.,data = trainingknn, tuneGrid= expand.grid(k=1:3),method = 'knn',trControl=trcontrol, preProc=c('center','scale'))
fitknn
plot(fitknn)
varImp(fitknn)
predictknn<- predict(fitknn,newdata = testknn)
predictknn
RMSE(predictknn,testknn$IMR)
par(mfrow=c(1,2))
#plot(fitknn, main="Graph for repeatedcv vs neighbours of IMR")
plot(predictknn~testknn$IMR, xlab="Actual IMR", ylab ="prediction", main="Graph for predition values of IMR on testing dataset")
abline(lm(predictknn~testknn$IMR),data=data2, col="RED")
summary(fitknn$finalModel)
fitknn$finalModel

##______Random Forest_______##

set.seed(7954)
ranrf<-sample(1:nrow(data2),0.8*nrow(data2))
ranrf
trainingrf <- data2[ranrf,]
testrf <- data2[-ranrf,]
View(trainingrf)
#trcontrolrf<-trainControl(method = 'oob')
trcontrolrf<-trainControl(method = 'repeatedcv', number = 10, repeats = 1000,search = "random")
set.seed(257)


tunegrid<-expand.grid(.mtry=c(1:15))
fitrf <- train(IMR ~.,data = training, tunegrid = tunegrid ,method = 'rf', trControl=trcontrol, ntree = 1000)
fitrf
fitrf$finalmodel
plot(fitrf)
varImp(fitrf)
predictrf<- predict(fitrf,newdata = testrf)
predictrf
RMSE(predictrf,testrf$IMR)
par(mfrow=c(1,2))
#plot(fitrf, main="Graph for repeatedcv vs forest of IMR")
plot(predictrf~testrf$IMR, xlab="Actual IMR", ylab ="prediction", main="Graph for predition values of IMR on testing dataset")
abline(lm(predictrf~testrf$IMR),data=A, col="RED")
summary(fitrf)
