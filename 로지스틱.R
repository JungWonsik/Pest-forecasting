data <-read.csv('C:/Users/user/Desktop/finall_linear.csv',header=T)
data <-read.csv('C:/Users/user/Desktop/finall_logistic.csv',header=T)

str(data)


##결측치 대치##
data <-read.csv('C:/Users/user/Desktop/linear_dummy.csv',header=T)
library(DMwR)
a<-knnImputation(data[,!names(data) %in% "y"])

install.packages("mice")
install.packages("jomo")
library(mice)
library(randomForest)
miceMod <-mice(data[,!names(data) %in% "y"],method='rf')
miceOutput<-complete(miceMod)
miceOutput
anyNA(miceOutput)
str(miceOutput)
miceOutput$피해발생위험지수
table(miceOutput$피해발생위험지수)
sum(is.na(miceOutput))
write.csv(miceOutput,file='C:/Users/user/Desktop/mice.csv')


##로지스틱용 데이터 생성##
data <-read.csv('C:/Users/user/Desktop/mice.csv',header=T)
data$y<-ifelse(data$피해발생위험지수>0,1,0)
str(data)
table(data$y)
data$y
write.csv(data,file='C:/Users/user/Desktop/finall_logistic.csv')
nrow(data)
length(data)

##오버샘플링##
data <-read.csv('C:/Users/user/Desktop/finall_logistic.csv',header=T)
data<-data[,-c(1,2,36,37,40,41)]
install.packages("DMwR")
library(DMwR)
data$y<-as.factor(data$y)
newdata<-SMOTE(y~.,data,perc.over = 500,perc.under=100)

library(caret)
x <- upSample(subset(data, select=-y), data$y)
x
table(data$y)
table(x$Class)

names(newdata)[1]
##정규성검사##
windows()
par(mfrow=c(3,3))
for(i in 1:9){
qqnorm(newdata[,i],main=names(newdata)[i])
}
windows()
par(mfrow=c(3,3))
for(i in 10:18){
  qqnorm(newdata[,i],main=names(newdata)[i])
}
windows()
par(mfrow=c(3,3))
for(i in 19:27){
  qqnorm(newdata[,i],main=names(newdata)[i])
}
windows()
par(mfrow=c(3,3))
for(i in 28:36){
  qqnorm(newdata[,i],main=names(newdata)[i])
}
newdata
##상관 분석##
cor_t <- cor(newdata)
install.packages("corrplot")
library(corrplot)
corrplot(cor_t)


##다중공선성검정##
newdata<-newdata[,-33]
model<-glm(y~.,data=newdata,family=binomial)
summary(model)
str(newdata)
library(car)
vif(model)
length(newdata)
str(newdata)
newdata<-newdata[,-34]
newdata<-newdata[,-5]
newdata<-newdata[,-19]
model<-glm(y~.,data=newdata,family=binomial)
summary(model)
vif(model)

##변수선택##
both <- step(model, direction="both")

newdata<-newdata[,-27]
##데이터 분할##
N=nrow(newdata)
tr.idx =sample(1:N, size=N * 0.7, replace= F)
train<-newdata[tr.idx,]
test<-newdata[-tr.idx,]

str(train)
##예측##
model1<-glm(y~ A_NUMPOINT + B_NUMPOINT + C_NUMPOINT + D_NUMPOINT + F_NUMPOINT + 
              G_NUMPOINT + m_count + 비율 + 산도 + 실제비료마그네슘사용량 + 
              실제비료석회사용량 + 실제비료칼리사용량 + 유효인산농도 + 
              최고기온..C. + 최다풍향.16방위. + 최대.풍속.m.s. + 평균.상대습도... + 
              평균.전운량.1.10. + 평균.지면온도..C. + 평균.풍속.m.s. + 
              평균.현지기압.hPa. + 평균기온..C. + 합계.일사.MJ.m2. + 지난해출현수 + 
              일강수량.mm. + 농사법_기타 + 농사법_무농약 + 시료명_유기물,data=train,family='binomial')
glm.prob<-predict(model1,newdata=test[,-31],type='response')
glm.pred<-test[,31]
glm.pred[glm.prob>0.5]='1'
table(glm.pred,test$y)

##roc그래프##
install.packages("pROC")
library(pROC)
library(MASS)
library(ROCR)
install.packages("Epi")
library(Epi)
pred <-prediction(glm.prob,glm.pred)
windows()
plot(performance(pred, "tpr", "fpr"))
performance(pred, "auc")
windows()
ROC(glm.prob,glm.pred)


table(newdata$y)

##원본 데이터 roc, auc##
model2<-glm(y~.,data=train,family=binomial)
summary(model2)
glm.prob<-predict(model2,newdata=test[,-31],type='response')
glm.pred<-test[,31]
glm.pred[glm.prob>0.5]='1'
table(glm.pred,test$y)
pred <-prediction(glm.prob,glm.pred)
performance(pred, "auc")
ROC(glm.prob,glm.pred)





##오버샘플링 안한 데이터##
str(data)
data<-data[,-29]

##데이터 분할##
N=nrow(data)
tr.idx =sample(1:N, size=N * 0.7, replace= F)
train<-data[tr.idx,]
test<-data[-tr.idx,]
model<-glm(y~.,data=newdata,family=binomial)
both <- step(model, direction="both")

str(test)
model1<-glm(y ~ A_NUMPOINT + B_NUMPOINT + C_NUMPOINT + D_NUMPOINT + F_NUMPOINT + 
              G_NUMPOINT + m_count + 비율 + 산도 + 실제비료마그네슘사용량 + 
              실제비료석회사용량 + 실제비료칼리사용량 + 유효인산농도 + 
              최고기온..C. + 최다풍향.16방위. + 최대.풍속.m.s. + 평균.상대습도... + 
              평균.전운량.1.10. + 평균.지면온도..C. + 평균.풍속.m.s. + 
              평균.현지기압.hPa. + 평균기온..C. + 합계.일사.MJ.m2. + 지난해출현수 + 
              일강수량.mm. + 농사법_기타 + 농사법_무농약 + 시료명_유기물,data=train,family='binomial')
glm.prob<-predict(model1,newdata=test[,-35],type='response')
glm.pred<-test[,35]
glm.pred[glm.prob>0.5]='1'
table(glm.pred,test$y)
pred <-prediction(glm.prob,glm.pred)
performance(pred, "auc")
windows()
ROC(glm.prob,glm.pred)
summary(model1)
