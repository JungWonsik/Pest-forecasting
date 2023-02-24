##mice로 결측치 대체한 데이터 - mice.csv##
data <-read.csv('C:/Users/user/Desktop/mice.csv',header=T)
str(data)
data<-data[,-c(1,35,36,39,40)]
str(data)

##상관분석##
library(corrplot)
cor_t<-cor(data[,-29])
windows()
corrplot(cor_t)

##해충이 발생한 지역만 추출##
newdata<-data[data$피해발생위험지수 > 0,]
str(newdata)

##다중공선성##
install.packages("car")
library(car)
model <-lm(피해발생위험지수~.,data=newdata)
summary(model)
newdata<-newdata[,-33]
model <-lm(피해발생위험지수~.,data=newdata)
summary(model)
coef(model)
vif(model)
str(newdata)
newdata<-newdata[,-20]
newdata<-newdata[,-19]
newdata<-newdata[,-6]
model <-lm(피해발생위험지수~.,data=newdata)
vif(model)
summary(model)
model<-lm(y~.,data=data)
summary(model)

windows()
par(mfrow=c(3,3))
for(i in 1:9){
  qqnorm(data[,i])
}

##변수선택##
both <- step(model, direction="both")
summary(both)

##로그화##
newdata$i<-transform(newdata$i,)

##데이터 분할##
N=nrow(newdata)
tr.idx =sample(1:N, size=N * 0.7, replace= F)
train<-newdata[tr.idx,]
test<-newdata[-tr.idx,]

str(test)
##회귀분석##
model1 <- lm(피해발생위험지수 ~ B_NUMPOINT + C_NUMPOINT + D_NUMPOINT + 
                       E_NUMPOINT + G_NUMPOINT + m_count + 비율 + 산도 + 실제비료석회사용량 + 
                       실제비료칼리사용량 + 유기물함량 + 유효규산농도 + 유효인산농도 + 
                       최고기온..C. + 최다풍향.16방위. + 평균.상대습도... + 평균.전운량.1.10. + 
                       평균.지면온도..C. + 평균.풍속.m.s. + 평균.현지기압.hPa. + 
                       평균기온..C. + 합계.일사.MJ.m2. + 지난해출현수 + 농사법_기타 + 
                       농사법_무농약 + 시료명_유기물 + 시료명_탄산염, data = train)
summary(model1)
result<-predict(model1,newdata=test[,-26])
result
test[,26]

sqrt(sum((result-test[,26])^2)/length(result))
IQR(test[,26])

##변수선택 안한 rmse##
model2<-lm(피해발생위험지수~.,data=train)
summary(model2)
result<-predict(model2,newdata=test[,-26])
sqrt(sum((result-test[,26])^2)/length(result))

##임의 변수선택 1##
model3 <- lm(피해발생위험지수 ~  m_count + 비율 + 산도 + 실제비료석회사용량 + 
                       실제비료칼리사용량 + 유기물함량 + 유효규산농도 + 유효인산농도 + 
                       최고기온..C. + 최다풍향.16방위. + 평균.상대습도... + 평균.전운량.1.10. + 
                       평균.지면온도..C. + 평균.풍속.m.s. + 평균.현지기압.hPa. + 
                       평균기온..C. + 합계.일사.MJ.m2. + 지난해출현수 + 농사법_기타 + 
                       농사법_무농약 + 시료명_유기물 + 시료명_탄산염, data = train)
result<-predict(model3,newdata=test[,-26])
sqrt(sum((result-test[,26])^2)/length(result))

##기온만 사용##
model4 <- lm(피해발생위험지수 ~ 최고기온..C. + 평균기온..C.,data=train)
result<-predict(model4,newdata=test[,-26])
sqrt(sum((result-test[,26])^2)/length(result))

##임의 변수선택 2##
model5 <- lm(피해발생위험지수 ~ B_NUMPOINT + C_NUMPOINT + D_NUMPOINT + 
                       E_NUMPOINT + G_NUMPOINT + m_count + 비율 + 산도 + 실제비료석회사용량 + 
                       실제비료칼리사용량 + 유기물함량 + 유효규산농도 + 유효인산농도 + 
                       최고기온..C. + 최다풍향.16방위. + 평균.상대습도... + 평균.전운량.1.10. + 
                       평균.지면온도..C. + 평균.풍속.m.s. + 평균.현지기압.hPa. + 
                       평균기온..C. + 합계.일사.MJ.m2. + 지난해출현수 + 
                       농사법_무농약 + 시료명_유기물 + 시료명_탄산염, data = train)
summary(model5)
result<-predict(model5,newdata=test[,-26])
result
test[,26]

sqrt(sum((result-test[,26])^2)/length(result))
