
###데이터 불러오기###

setwd("C:/Users/user/Desktop/새 폴더 (2)")
data <- read.csv('mice_v.csv',header=T)



###지난해출현수와 피해발생위험지수 산점도###

plot(x = data$지난해출현수, y = data$피해발생위험지수, ylab = "피해발생위험지수", xlab = "지난해 출현수", main = "산점도")


par(mfrow=c(3,1))
###시군구별 평균온도###
install.packages("RColorBrewer")
library(RColorBrewer)
str(data)
data1 <- data[,c(26,42)]
data2 <- tapply(data1$평균기온..C.,data1$시군구명,mean)
data3 <-sort(data2, decreasing = TRUE)
barplot(data3,main="시군구별 평균온도",col="lightblue",las=2,ylab="평균온도..C")


###시군구별 강수량###
data4 <- data[,c(30,42)]
data5 <- tapply(data4$일강수량.mm.,data4$시군구명,mean)
data6 <-sort(data5, decreasing = TRUE)
barplot(data6,main="시군구별 일강수량",col="lightblue",las=2,ylab="일강수량(mm)")


###시군구별 피해발생위험지수###
data1 <- data[,c(38,42)]
data2 <- tapply(data1$피해발생위험지수,data1$시군구명,mean)
data3 <-sort(data2, decreasing = TRUE)
barplot(data3,main="시군구별 피해발생위험지수",col="lightblue",las=2,ylab="피해발생위험지수")


