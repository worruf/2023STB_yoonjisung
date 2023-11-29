response<-c(2,3,4,4,5,6,6,7,8)
x<-rep(c("model","model2","model3"),c(3,3,3))
levels=x
ano.result<-aov(response~levels)
summary(ano.result)
