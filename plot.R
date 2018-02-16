wan1<- read.csv(file = '2011绘图.csv')
#wan1<- read.csv(file = '2013绘图.csv')
wan1<-wan1[order(wan1$in.),]
#wan2<- wan2[order(wan2$in.),]
wan1.urban<- wan1[wan1$urban==1,]
wan1.rural<- wan1[wan1$urban==0,]
f<- quantile(wan1.rural$in.,probs = seq(from = 0,to = 1,length.out = 51))
#wan2.urban<- wan2[wan2$urban==1,]
#wan2.rural<- wan2[wan2$urban==0,]
m1<- matrix(nrow = 50,ncol = 3)#2011Urban
m2<- matrix(nrow = 50,ncol = 3)#2011Rural
for(i in 1:50){
  low<- f[i]
  up<- f[i+1]
  junzhi.urban1<- weighted.mean(wan1.urban$in.[wan1.urban$in.>=low&wan1.urban$in.<=up],w=wan1.urban$weight[wan1.urban$in.>=low&wan1.urban$in.<=up])
  junzhi.urban2<- weighted.mean(wan1.urban$Co2[wan1.urban$in.>=low&wan1.urban$in.<=up],w=wan1.urban$weight[wan1.urban$in.>=low&wan1.urban$in.<=up])
  junzhi.urban3<- mean(wan1.urban$weight[wan1.urban$in.>=low&wan1.urban$in.<=up])
  junzhi.rural1<- weighted.mean(wan1.rural$in.[wan1.rural$in.>=low&wan1.rural$in.<=up],w=wan1.rural$weight[wan1.rural$in.>=low&wan1.rural$in.<=up])
  junzhi.rural2<- weighted.mean(wan1.rural$Co2[wan1.rural$in.>=low&wan1.rural$in.<=up],w=wan1.rural$weight[wan1.rural$in.>=low&wan1.rural$in.<=up])
  junzhi.rural3<- mean(wan1.rural$weight[wan1.rural$in.>=low&wan1.rural$in.<=up])
  m1[i,1]<- junzhi.urban1#shouru
  m1[i,2]<- junzhi.urban2#co2
  m1[i,3]<- junzhi.urban3
  m2[i,1]<- junzhi.rural1#shouru
  m2[i,2]<- junzhi.rural2#co2
  m2[i,3]<- junzhi.rural3
  m1
  m2
}
m1<- as.data.frame(m1)
names(m1)<- c('收入','co2','权重')
m1$收入方<- (m1$收入)^2
m2<- as.data.frame(m2)
names(m2)<- c('收入','co2','权重')
m2$收入方<- (m2$收入)^2
#write.csv(m1,file = '2011城镇.csv')
#write.csv(m2,file = '2011农村.csv')

#wan1fit1<- wan1.rural[,c(-1,-16)]
#w1<- wan1.rural[,16]
#wan1fit[,c(1:7)]<- scale(wan1fit[,c(1:7)],center = T,scale = T)
#lo1<- lm(wanfit1$Co2~.,data = wan1fit1,weights = w1)

lo1<- lm(m1$co2~m1$收入+m1$收入方,data= m1,weights = m1$权重)#2011urban
lo2<- lm(m2$co2~m2$收入+m2$收入方,data = m2,weights = m2$权重)#2011rural
plot(m1$co2~m1$收入,col='blue')
#2011urban
#mean(m1$收入方)#13.1462
#model1<- function(x){376.605+117.863*x-2.738*13.1462}
M.loess1<- loess(m1$co2~m1$收入)
Fit1<- fitted(M.loess1)
lines(m1$收入,Fit1)
#fit1<- model1(f)
#lines(fit1~f)
#mean(m2$收入方)#5.565491
#model2<- function(x){306.849+19.817*x+1.153*5.565491}
#fit2<- model2(f)
points(m2$co2~m2$收入,col='red')
#lines(fit2~f)
M.loess2<- loess(m2$co2~m2$收入)
fit2<- fitted(M.loess2)
lines(m2$收入,fit2)
#new<- as.data.frame(wan1fit1[,c(2:14)])
#new<- as.data.frame(new[new$in.%in%f,])
#abline(lo1)

#fit1<- predict(lm(wan1fit1$Co2~.,data = wan1fit1,weights = w1),newdata = new,interval =  "prediction")
#lines(fit1~new$in.)



wan2<- read.csv(file = '2013绘图.csv')

wan2<- wan2[order(wan2$in.),]
wan2.urban<- wan2[wan2$urban==1,]
wan2.rural<- wan2[wan2$urban==0,]


m3<- matrix(nrow = 50,ncol = 3)#2013Urban
m4<- matrix(nrow = 50,ncol = 3)#2013Rural
for(i in 1:50){
  low<- f[i]
  up<- f[i+1]
  junzhi.urban1<- weighted.mean(wan2.urban$in.[wan2.urban$in.>=low&wan2.urban$in.<=up],w=wan2.urban$weight[wan2.urban$in.>=low&wan2.urban$in.<=up],na.rm = T)
  junzhi.urban2<- weighted.mean(wan2.urban$Co2[wan2.urban$in.>=low&wan2.urban$in.<=up],w=wan2.urban$weight[wan2.urban$in.>=low&wan2.urban$in.<=up],na.rm = T)
  junzhi.urban3<- mean(wan2.urban$weight[wan2.urban$in.>=low&wan2.urban$in.<=up],na.rm = T)
  junzhi.rural1<- weighted.mean(wan2.rural$in.[wan2.rural$in.>=low&wan2.rural$in.<=up],w=wan2.rural$weight[wan2.rural$in.>=low&wan2.rural$in.<=up],na.rm = T)
  junzhi.rural2<- weighted.mean(wan2.rural$Co2[wan2.rural$in.>=low&wan2.rural$in.<=up],w=wan2.rural$weight[wan2.rural$in.>=low&wan2.rural$in.<=up],na.rm = T)
  junzhi.rural3<- mean(wan2.rural$weight[wan1.urban$in.>=low&wan2.rural$in.<=up],na.rm = T)
  m3[i,1]<- junzhi.urban1#shouru
  m3[i,2]<- junzhi.urban2#co2
  m3[i,3]<- junzhi.urban3
  m4[i,1]<- junzhi.rural1#shouru
  m4[i,2]<- junzhi.rural2#co2
  m4[i,3]<- junzhi.rural3
  m3
  m4
}
m3<- as.data.frame(m3)
names(m3)<- c('收入','co2','权重')#2013urban
m3$收入方<- (m3$收入)^2
m4<- as.data.frame(m4)#2013rural
names(m4)<- c('收入','co2','权重')
m4$收入方<- (m4$收入)^2
#2013urban
points(m3$co2~m3$收入,col='green')
#lo3<- lm(m3$co2~m3$收入+m3$收入方,data= m3,weights= m3$权重)
#mean(m3$收入方,na.rm=T)
#model3<- function(x){549.6651+59.1501*x-10283*59.06914}
#fit3<- model3(f)
#lines(fit3~f)
#lo4<- lm(m4$co2~m4$收入+m4$收入方,data= m4,weights= m4$权重)
#mean(m4$收入方,na.rm=T)
#model4<- function(x){459.412+66.195*x-1.378*8.129068}
M.loess3<- loess(m3$co2~m3$收入)
fit3<- fitted(M.loess3)
lines(m3$收入,fit3)

points(m4$co2~m4$收入,col='orange')
M.loess4<- loess(m4$co2~m4$收入)
fit4<- fitted(M.loess4)
lines(m4$收入,fit4)



write.csv(f,file = '分位数表.csv')
write.csv(m1,file = '2011城镇.csv')
write.csv(m2,file = '2011农村.csv')
write.csv(m3,file = '2013城市.csv')
write.csv(m4,file = '2013农村.csv')






#多元线性回归
wan1<- read.csv(file = '2011绘图.csv')
wan1fit<- wan1[,-c(1,16)]
lf1<- lm(wan1$Co2~.,data = wan1fit,weights = wan1$weight)
summa