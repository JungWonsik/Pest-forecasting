#finall_logistic 종속변수 t검정

data = read.csv('C:/Users/user/Desktop/finall_logistic.csv')
data
summary(data)

cn<-colnames(data)
cn <- cn[-c(1,2,9,33,34,35,36,37,38,39,40,41,42)]

cn
treturn<-lapply(cn,
                function(x){
                  tw=var.test(data[[x]]~ data$y)
                  varequal<- ifelse(tw$p.value>0.05, TRUE, FALSE)
                  tx=t.test(data[[x]]~ data$y,var.equal=varequal)
                  return(data.frame(name=x,
                                    group1.mean=tx$estimate[1],
                                    group2.mean=tx$estimate[2],
                                    f.pvalue= tw$p.value,
                                    t.value= tx$statistic,
                                    t.pvalue= tx$p.value,
                                    t.method= substr(tx$method,1,12)
                  ))
                })
t.result<- NULL
rownames(t.result)<-NULL
for(i in 1:length(treturn)){
  t.result<- rbind(t.result, treturn[[i]])
}
t.result

