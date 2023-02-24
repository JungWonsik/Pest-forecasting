final_logistic = read.csv("finall_logistic.csv")

attach(final_logistic)

chisq.test(G_NUMPOINT,y)
chisq.test(농사법_기타,y)
chisq.test(농사법_무농약,y)
chisq.test(농사법_유기농,y)
chisq.test(시료명_기타,y)
chisq.test(시료명_뼈,y)
chisq.test(시료명_유기물,y)
chisq.test(시료명_탄산염,y)
chisq.test(시료명_토기,y)
chisq.test(시료명_토양,y)