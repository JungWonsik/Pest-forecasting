
install.packages("ggplot2")
library(ggplot2)
spot_data = read.csv('C:/Users/user/Desktop/새 폴더/병해충발생_지점정보_2018.csv')
spot_data
summary(spot_data)
windows()
ggplot(spot_data, aes(x=시도명)) + geom_bar(aes(fill=작물유형명)) + labs(fill = '작물유형명') + 
  xlab('시도명') + ylab('작물수') + ggtitle('시도별 경작 작물 수') + 
  theme(plot.title=element_text( face="bold", size=30, vjust=2))
