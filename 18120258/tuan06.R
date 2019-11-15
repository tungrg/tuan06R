setwd('D:/18120258')
getwd()
load('heights.rda')
summary(heights)
hist(heights)
t.test(heights,mu = 160, conf.level=0.95)
#######
data<-read.csv("profit.csv", header = TRUE)
attach(data)
x<-data$profit
hist(x)
t.test(x,alternative = "less", mu = 65, conf.level = 0.99)
t.test(x,alternative = "two.sided", mu = 60, conf.level=0.99)
########
ci.mean = function(x){
 s = sqrt(var(x))
 z = qnorm(1-alpha/2)
 e = z*s/sqrt(length(x))
}
test.geq.oneside = function(x,nuy,alpha){
	s = sqrt(var(x))
	meantb = mean(x)
	t0 = (meantb - nuy)/(s/sqrt(length(x)))
	ttin = qt(1-alpha,length(x) - 1)
	if(t0 < (-ttin))
		{cat("Bac bo")}
	else
		{cat("khong du co so de bac bo")}
}
x<-c(5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10)
test.geq.oneside(x,8,0.05)
