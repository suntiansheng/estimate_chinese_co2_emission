#1995
q1995<- read.csv('中间投入.csv',header = F)
Q1995<- read.csv('总产出.csv',header = F)
q<- as.matrix(q1995)
Q1995<- as.vector(t(Q1995))
Q<- diag(Q1995)
A<- q%*%solve(Q)
I<- diag(rep(1,32))
B1<- solve(I-A)
B<- B1-I
write.csv(B,file = 'complete consumption coefficient2007.csv')
real<- read.csv(file = 'real.csv',header = F)
real<- as.matrix(real)
max(B-real)
write.csv(B-real,file = '误差项.csv')



#计算人均
Y<- read.csv('Y.csv',header = T)
Y1<- Y[,-(1:3)]
P<- read.csv('pollution.csv')
str(Y1)
Y1<- as.matrix(Y1)
str(P)
P<- as.matrix(P)



#zhuanghuan gongzuo lujin
q1995<- read.csv('中间投入2008.csv',header = F)
Q1995<- read.csv('总产出2008.csv',header = F)
q<- as.matrix(q1995)
Q1995<- as.vector(t(Q1995))
Q<- diag(Q1995)
A<- q%*%solve(Q)
I<- diag(rep(1,32))
B1<- solve(I-A)
B<- B1-I

PP<- Y1%*%B1
PP1<- PP%*%P
PP2<- B1%*%P
write.csv(PP1,file  = '每人的污染排放2008.csv')
write.csv(PP2,file = '污染强度2008.csv')
##
shouru<- read.csv(file = '2011画图初始.csv')
wuran<- read.csv(file = '每人的污染排放2011.csv')
wanzheng<- merge(shouru,wuran,by = 'householdID')

plot(wanzheng$CO2.x~wanzheng$总收入..购买.经营.)
#对总收入（-购买-经营）处理
##去除极端值

wan1<- wanzheng[wanzheng$总收入..购买.经营.>=0&wanzheng$无回答个数<25,]
plot(wan1$CO2.x)
plot(wan1$总收入..购买.经营.)
mean(wan1$CO2.x)+2*sd(wan1$CO2.x)
#40894.82
mean(wan1$CO2.x)-1.5*sd(wan1$CO2.x)
#-19306.38
mean(wan1$总收入..购买.经营.)+2*sd(wan1$总收入..购买.经营.)
#101091.6
mean(wan1$总收入..购买.经营.)-1.5*sd(wan1$总收入..购买.经营.)
#-27988.44
wan1<- wan1[wan1$CO2.x<40894.82&wan1$总收入..购买.经营.<101091.6,]

plot(wan1$CO2.y~wan1$总收入..购买.经营.,xlim=c(0,150000),ylim=c(0,70000))

#拟合直线
modle1<- lm(wan1$CO2.y~wan1$总收入..购买.经营.)
summary(modle1)
abline(modle1,col="red")
fit.x<- wan1$总收入..购买.经营.
plot(fit.num~modle1$fitted.values)
#排序
ord1<- order(wan1$总收入..购买.经营.)
wan1<-wan1[ord1,]
modle1$fitted.values<- sort(modle1$fitted.values)
#找接近值
m<-vector(length =length(wan1$总收入..购买.经营.) )
for(i in 1:length(wan1$总收入..购买.经营.)){
  l<- abs(wan1$CO2.y[i]-modle1$fitted.values[i])
  if(l<=100){m[i]<-wan1$householdID[i]}
}


fit.id <- unique(m)
fit.id <- fit.id[2:length(fit.id)]
data.value<-  wan1[wan1$householdID%in%fit.id,]
plot(data.value$CO2.y~data.value$总收入..购买.经营.)





#第二种画法
shouru<- read.csv(file = '2011画图初始.csv')
wuran<- read.csv(file = '每人的污染排放2011.csv')
wanzheng<- merge(shouru,wuran,by = 'householdID')
wan1<- wanzheng[wanzheng$总收入..经营.>=0&wanzheng$无回答个数<25,]
#wan1<- wan1[wan1$CO2.x<40894.82&wan1$总收入..购买.经营.<101091.6,]
wan1<-wan1[order(wan1$总收入..经营.),]
m<- matrix(nrow = 100,ncol = 2)
l<- length(wan1$总收入..经营.)
s<- floor(l/100)
for(i in 0:99){
  low<- i*s+1
  up<- (i+1)*s
  junzhi1<- mean(wan1$总收入..经营.[c(low:up)])
  junzhi2<- mean(wan1$CO2.y[c(low:up)])
  m[i+1,1]<- junzhi1#shouru
  m[i+1,2]<- junzhi2#co2
  m
}
plot(m[,1]~m[,2])
lo1<- loess(m[,1]~m[,2])
lines(lo1)
library(ggplot2)
m<- as.data.frame(m)
ggplot(m, aes(x=m$V1, y=m$V2))+geom_point()+stat_smooth()
plot(m$V2~m$V1)



#第三种画法

shouru<- read.csv(file = '2011画图初始.csv')
wuran<- read.csv(file = '每人的污染排放2011.csv')
wanzheng<- merge(shouru,wuran,by = 'householdID')
wan1<- wanzheng[wanzheng$总收入..经营.>=0&wanzheng$无回答个数<25,]
#wan1<- wan1[wan1$CO2.x<40894.82&wan1$总收入..购买.经营.<101091.6,]
wan1<-wan1[order(wan1$总收入..经营.),]
wan1.urban<- wan1[wan1$area=='Urban',]
wan1.rural<- wan1[wan1$area=='Rural',]
m1<- matrix(nrow = 100,ncol = 2)#Urban
m2<- matrix(nrow = 100,ncol = 2)#Rural
l1<- length(wan1.urban$总收入..经营.)
l2<- length(wan1.rural$总收入..经营.)
s1<- floor(l1/100)
s2<- floor(l2/100)
for(i in 0:99){
  low1<- i*s1+1
  up1<- (i+1)*s1
  low2<- i*s2+1
  up2<- (i+1)*s2
  junzhi.urban1<- mean(wan1.urban$总收入..经营.[c(low1:up1)])
  junzhi.urban2<- mean(wan1.urban$CO2.y[c(low1:up1)])
  junzhi.rural1<- mean(wan1.rural$总收入..经营.[c(low2:up2)])
  junzhi.rural2<- mean(wan1.rural$CO2.y[c(low2:up2)])
  m1[i+1,1]<- junzhi.urban1#shouru
  
  m1[i+1,2]<- junzhi.urban2#co2
  m2[i+1,1]<- junzhi.rural1#shouru
  m2[i+1,2]<- junzhi.rural2#co2
  m1
  m2
}
m1<- as.data.frame(m1)
names(m1)<- c('收入','co2')
m1$区域<- 1#城市
m2<- as.data.frame(m2)
names(m2)<- c('收入','co2')
m2$区域<- 2#农村
m<- rbind(m1,m2)
lo1<- nls(m1$co2~a*(m1$收入^2)+b*m1$收入+c,data = m1,start = c(a=0,b=0,c=0))
summary(lo1)
lo2<- nls(m2$co2~a*(m2$收入^2)+b*m2$收入+c,data = m2,start = c(a=0,b=0,c=0))
summary(lo2)
library(ggplot2)
p1<- ggplot(m,aes(x=m$收入,y=m$co2))
p1+geom_point(col=m$区域)

points(m2[,1]~m2[,2],col='red')
lo1<- nls(m1$V2~a*(m1$V1^2)+b*m1$V1+c,data = m,start = c(a=0,b=0,c=0))

lines()
library(ggplot2)
m1<- as.data.frame(m1)
m2<- as.data.frame(m2)
ggplot(m1, aes(x=m$V1, y=m$V2))+geom_point()+stat_smooth()+geom_point(m2, aes(x=m2$V1,y=m2$V2))
plot(m$V2~m$V1)

