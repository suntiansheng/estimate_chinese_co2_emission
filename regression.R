wan1<- read.csv(file = '2011»æÍ¼.csv')
wan1fit<- wan1[,-c(1,16)]
lf1<- lm(wan1fit$Co2~.,data = wan1fit,weights = wan1$weight)
summary(lf1)
wan2<- read.csv(file = '2013»æÍ¼.csv')
wan2fit<- wan2[,-c(1,16)]
lf2<- lm(wan2fit$Co2~.,data = wan2fit,weights = wan2$weight)
summary(lf2)
